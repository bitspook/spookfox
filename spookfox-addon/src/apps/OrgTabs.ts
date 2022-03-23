import { Draft, freeze, Immutable } from 'immer';
import { SFApp, SFEvents, Spookfox } from '~src/Spookfox';
import { gobbleErrorsOf, sleep } from '~src/lib';

export type OrgTabsState = Immutable<{
  // All the tabs open in browser at any time. Assumption is that this list is
  // created when Emacs first connects, and then kept up-to-date by browser
  // itself whenever anything related to a tab changes
  openTabs: { [id: string]: OpenTab };
  // All tabs which are saved in Emacs. We obtain this list when Emacs first
  // connects. After that, any time something related to saved tabs changes,
  // this list should be updated *after the face*. i.e make the change in Emacs,
  // and then ask Emacs how this list looks like; either by asking for the whole
  // list again, or designing the response such that Emacs returns the updated
  // `SFTab`
  savedTabs: { [id: string]: SavedTab };
  // Firefox containers, when configured to "auto-close tabs" cause a
  // race-condition where they rapidly create+close+create tabs. In this case,
  // the close callback gets called very quickly. On tab create, Emacs save the
  // tab and respond with the saved tab. But before it can do this, Firefox has
  // already closed the tab and emitted onClosed event. onClosed checks if we
  // already saved the tab, and since saving the tab hasn't finished yet,
  // concludes that we haven't. So this rapidly closed tab don't get removed
  // from Emacs. To resolve this, we need a `savingTabs` which keeps track if
  // tabs which are under-process of being saved.
  savingTabs: number[];
  // Let's maintain a list of tab we are reopening, so the 'new tab' handler
  // knows whether to instruct Emacs to create a new org entry, or update an
  // existing one
  // Please read 'Reopening a saved tab' flow in architecture doc
  reOpeningTabs: Array<{ id: string; url: string }>;
}>;

/**
 * A tab saved in Emacs.
 * We need to maintain our own mapping of which tab is saved where in
 * Emacs because browser don't provide a way to uniquely identify tabs.
 * `browser.tabs.Tab.id` is ephemeral, and gets reset every time browser
 * window is reopened.
 */
export interface SavedTab {
  // A unique ID created by Emacs, because browser's tab IDs are ephemeral
  id?: string;
  title: string;
  url: string;
  // Any changes made to a chained tab will be synced back to Emacs
  chained?: boolean;
}

/**
 * A tab open in browser which might have a saved instance in Emacs.
 */
export interface OpenTab {
  browserTabId: number;
  savedTabId?: string;
  chained?: boolean;
}

/**
 * A convenient function to convert an `OpenTab` to `SavedTab` for when we need
 * to communicate with Emacs.
 */
export const fromBrowserTab = (
  tab: Partial<browser.tabs.Tab & OpenTab>
): SavedTab => {
  return {
    id: tab.savedTabId,
    // I've found tridactyl to send a single ASCII 160 (non-breaking-space) in
    // title; which Emacs string trimming functions don't consider a space. One
    // flimsy way to handle it is to just not send an empty title 🤷🏻
    title: tab.title.trim() ? tab.title : '<no-title>',
    url: tab.url,
    chained: tab.chained || false,
  };
};

export default class OrgTabs implements SFApp<OrgTabsState> {
  initialState: Immutable<OrgTabsState> = freeze(
    {
      openTabs: {},
      savedTabs: {},
      savingTabs: [],
      reOpeningTabs: [],
    },
    true
  );

  get state(): OrgTabsState {
    return this.sf.state[this.name];
  }

  dispatch(name: Actions, payload: unknown) {
    return this.sf.dispatch(`${this.name}/${name}`, payload);
  }

  constructor(public name: string, public sf: Spookfox) {
    this.name = name;
    this.sf = sf;

    sf.addEventListener(
      SFEvents.EMACS_CONNECTED,
      gobbleErrorsOf(this.handleConnected)
    );
    browser.tabs.onCreated.addListener(gobbleErrorsOf(this.handleNewTab));
    browser.tabs.onUpdated.addListener(gobbleErrorsOf(this.handleUpdateTab));
    browser.tabs.onRemoved.addListener(gobbleErrorsOf(this.handleRemoveTab));

    sf.registerReqHandler(
      EmacsRequests.GET_ACTIVE_TAB,
      gobbleErrorsOf(this.getActiveTab)
    );
    sf.registerReqHandler(
      EmacsRequests.GET_ALL_TABS,
      gobbleErrorsOf(this.getAllTabs)
    );
    sf.registerReqHandler(EmacsRequests.OPEN_TAB, gobbleErrorsOf(this.openTab));
    sf.registerReqHandler(
      EmacsRequests.OPEN_TABS,
      gobbleErrorsOf(this.openTabs)
    );
    sf.registerReqHandler(
      EmacsRequests.SEARCH_FOR,
      gobbleErrorsOf(this.openSearchTab)
    );
  }

  /**
   * Get the active tab of first browser window. Let's not go in the nuances of
   * multiple browser windows just yet. For now, we work with the first browser
   * window that was opened. In Firefox, this window gets an id=1. Might need to
   * check this behavior when we are adding chrome support.
   */
  getActiveTab = async (): Promise<SavedTab> => {
    const tabs = await browser.tabs.query({ windowId: 1, active: true });

    if (!tabs.length) {
      // Probably shouldn't be doing this, but just throwing an error and calling it
      // a day simplifies the types a lot. Besides I am not sure if there will ever
      // be a case when a window don't have an active tab. This check is here because
      // doing `tabs[0]` give me heebie-jeebies
      throw new Error('No active tab found');
    }

    return fromBrowserTab(tabs[0]);
  };

  openSearchTab = async (p: string) => {
    (browser as any).search.search({ query: p });

    return {};
  };

  /**
   * Get all tabs which are open in first browser window. Follows same semantics
   * as `getActiveTab`
   */
  getAllTabs = async (): Promise<SavedTab[]> => {
    const tabs = await browser.tabs.query({ windowId: 1 });

    return tabs.map(fromBrowserTab);
  };

  openTab = async (p: { id?: string; url: string }): Promise<SavedTab> => {
    if (p.id) {
      const tab = Object.values(this.state.openTabs).find(
        (t) => t?.savedTabId === p.id
      );

      if (tab) {
        try {
          await browser.tabs.update(tab.browserTabId, { active: true });
          return null;
        } catch (err) {
          if (/invalid tab id/.test(err.message.toLowerCase())) {
            this.dispatch(Actions.REMOVE_TAB_START, {
              tabId: tab.browserTabId,
            });
          } else {
            console.error('Error while opening-tab', { payload: p, err });
          }
        }
      }

      this.dispatch(Actions.REOPEN_TAB, p);
    }

    const tab = await browser.tabs.create({ url: p.url });

    return fromBrowserTab(tab);
  };

  openTabs = async (
    tabs: {
      id: string;
      url: string;
    }[]
  ): Promise<SavedTab[]> => {
    const openedTabs = await Promise.all(tabs.map(this.openTab));

    return openedTabs;
  };

  private handleNewTab = async (tab: browser.tabs.Tab) => {
    this.dispatch(Actions.SAVE_TAB_START, tab.id);

    // This might be one of the places where browser compatibility is an issue
    // Please read 'Reopening a saved tab' flow in architecture doc
    const reOpeningTab = this.state.reOpeningTabs.find(
      ({ url }) => url.includes(tab.title) && tab.url === 'about:blank'
    );
    if (reOpeningTab) {
      return;
    }

    try {
      const savedTab = (await this.sf.request(
        EmacsRequests.TOGGLE_TAB_CHAINING,
        fromBrowserTab(tab)
      )) as SavedTab;
      const openedTab: OpenTab = {
        browserTabId: tab.id,
        savedTabId: savedTab.id,
      };

      this.dispatch(Actions.SAVE_TAB_SUCCESS, { savedTab, openedTab });
    } catch (error) {
      console.error('Failed TOGGLE_TAB_CHAINING [err=', error, ']');
      this.dispatch(Actions.SAVE_TAB_FAIL, { tab, error });
    }
  };

  private handleUpdateTab = async (
    tabId: number,
    patch: {
      audible?: boolean;
      discarded?: boolean;
      favIconUrl?: string;
      mutedInfo?: browser.tabs.MutedInfo;
      pinned?: boolean;
      status?: string;
      title?: string;
      url?: string;
    }
  ) => {
    // This might be one of the places where browser compatibility is an issue
    // Please read 'Reopening a saved tab' flow in architecture doc
    const reOpeningTab = this.state.reOpeningTabs.find(
      ({ url }) => url === patch.url
    );
    if (reOpeningTab) {
      const tab = await browser.tabs.get(tabId);
      const savedTab = this.state.savedTabs[reOpeningTab.id];
      const openedTab: OpenTab = {
        browserTabId: tab.id,
        savedTabId: savedTab?.id,
      };
      this.dispatch(Actions.SAVE_TAB_SUCCESS, { savedTab, openedTab });
    }

    // We aren't interested in all the tab changes (e.g which tab is active).
    // Here is a list of properties which we care about
    const desiredProps = ['url', 'title'];
    const changedDesiredProps = Object.keys(patch).filter((k) =>
      desiredProps.includes(k)
    );
    if (!changedDesiredProps.length) {
      return;
    }

    const openedTab = this.state.openTabs[`${tabId}`];
    const savedTab = this.state.savedTabs[openedTab?.savedTabId];

    this.dispatch(Actions.UPDATE_TAB_START, { tabId, patch });

    if (savedTab?.chained) {
      try {
        const updatedTab = await this.sf.request('UPDATE_TAB', {
          id: savedTab.id,
          patch: desiredProps.reduce((accum, prop) => {
            // Make sure we aren't sending unnecessary props to Emacs (e.g
            // patch.status)
            if (patch[prop]) accum[prop] = patch[prop];
            return accum;
          }, {}),
        });
        this.dispatch(Actions.UPDATE_TAB_SUCCESS, updatedTab);
      } catch (error) {
        console.error(
          `Error during updating tabs. [tabId=${tabId}, error=${error}]`
        );
        this.dispatch(Actions.UPDATE_TAB_FAIL, { error, tabId });
      }
    } else {
      this.dispatch(Actions.UPDATE_TAB_SUCCESS, undefined);
    }
  };

  private handleRemoveTab = async (tabId: number, { windowId }) => {
    if (windowId !== 1) return;
    if (this.state.savingTabs.includes(tabId)) await sleep(600);

    const savedTabId = this.state.openTabs[`${tabId}`]?.savedTabId;
    this.dispatch(Actions.REMOVE_TAB_START, { tabId });

    const savedTab = this.state.savedTabs[savedTabId];

    if (savedTab?.chained) {
      try {
        const removedTab = await this.sf.request('REMOVE_TAB', {
          id: savedTabId,
        });
        this.dispatch(Actions.REMOVE_TAB_SUCCESS, removedTab);
      } catch (error) {
        console.error(
          `Error during removing tab. [tabId=${tabId}, error=${error}]`
        );
        this.dispatch(Actions.REMOVE_TAB_FAIL, { tabId, error });
      }
    }
  };

  /**
   * Initialize the state when Emacs first connects.
   */
  private handleConnected = async () => {
    const savedTabs = (await this.sf.request('GET_SAVED_TABS')) as SavedTab[];
    const currentTabs = await browser.tabs.query({ windowId: 1 });

    // Problem: There might be tabs with same URLs
    // Solution: First open tab in browser is mapped to first tab saved in Emacs.
    // Catch: Every time this function runs, all current tabs which match urls
    // saved in Emacs are mapped; regardless of whether user meant it or not.
    const takenSavedTabIds = [];
    const openTabs = currentTabs.reduce((accum, tab) => {
      const savedTab = savedTabs.find(
        (st) => st.url === tab.url && takenSavedTabIds.indexOf(st.id) === -1
      );
      const openTab: OpenTab = {
        browserTabId: tab.id,
      };

      if (savedTab) {
        takenSavedTabIds.push(savedTab.id);
        openTab.savedTabId = savedTab.id;
        openTab.chained = savedTab.chained;
      }

      accum[tab.id] = openTab;
      return accum;
    }, {} as { [id: string]: OpenTab });

    const savedTabsMap = savedTabs.reduce((accum, tab) => {
      accum[tab.id] = tab;
      return accum;
    }, {});

    this.dispatch(Actions.EMACS_RECONNECTED, {
      openTabs,
      savedTabs: savedTabsMap,
    });
  };

  reducer({ name, payload }, state: Draft<OrgTabsState>) {
    switch (name) {
      case Actions.EMACS_RECONNECTED: {
        state.openTabs = payload.openTabs;
        state.savedTabs = payload.savedTabs;
        break;
      }

      case Actions.SAVE_TAB_START:
        state.savingTabs.push(payload);
        break;

      case Actions.SAVE_TAB_SUCCESS: {
        state.savedTabs[payload.savedTab.id] = payload.savedTab;
        state.openTabs[payload.openedTab.browserTabId] = payload.openedTab;
        state.savingTabs = state.savingTabs.filter(
          (id) => id !== payload.openedTab.id
        );
        state.reOpeningTabs = state.reOpeningTabs.filter(
          ({ id }) => id !== payload.savedTab.id
        );
        break;
      }

      case Actions.UPDATE_TAB_START:
        state.openTabs[payload.tabId] = {
          ...state.openTabs[`${payload.tabId}`],
          ...payload.patch,
        };
        break;

      case Actions.UPDATE_TAB_SUCCESS:
        if (payload) state.savedTabs[payload.id] = payload;
        break;

      case Actions.REMOVE_TAB_START: {
        delete state.openTabs[`${payload.tabId}`];
        state.savingTabs = state.savingTabs.filter(
          (tId) => tId !== payload.tabId
        );
        break;
      }

      case Actions.REMOVE_TAB_SUCCESS:
        delete state.savedTabs[payload.id];
        break;

      case Actions.REOPEN_TAB:
        state.reOpeningTabs = [...state.reOpeningTabs, payload];
        break;
    }

    return state;
  }
}

export enum Actions {
  EMACS_RECONNECTED = 'EMACS_RECONNECTED',
  SAVE_TAB_START = 'SAVE_TAB_START',
  SAVE_TAB_SUCCESS = 'SAVE_TAB_SUCCESS',
  SAVE_TAB_FAIL = 'SAVE_TAB_FAIL',
  UPDATE_TAB_START = 'UPDATE_TAB_START',
  UPDATE_TAB_SUCCESS = 'UPDATE_TAB_SUCCESS',
  UPDATE_TAB_FAIL = 'UPDATE_TAB_FAIL',
  REMOVE_TAB_START = 'REMOVE_TAB_START',
  REMOVE_TAB_SUCCESS = 'REMOVE_TAB_SUCCESS',
  REMOVE_TAB_FAIL = 'REMOVE_TAB_FAIL',
  REOPEN_TAB = 'REOPEN_TAB',
}

export enum EmacsRequests {
  TOGGLE_TAB_CHAINING = 'TOGGLE_TAB_CHAINING',
  GET_ACTIVE_TAB = 'GET_ACTIVE_TAB',
  GET_ALL_TABS = 'GET_ALL_TABS',
  OPEN_TAB = 'OPEN_TAB',
  OPEN_TABS = 'OPEN_TABS',
  SEARCH_FOR = 'SEARCH_FOR',
}
