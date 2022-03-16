import { sleep } from './lib';
import {
  Spookfox,
  SFEvent,
  SFEvents,
  State,
  Actions,
  EmacsRequests,
} from './Spookfox';
import {
  SFTab,
  fromBrowserTab,
  getActiveTab,
  getAllTabs,
  openTab,
  openTabs,
  OpenTab,
} from './tabs';

const searchFor = async (p: string) => {
  browser.search.search({ query: p });

  return {};
};

const run = async () => {
  const sf = ((window as any).sf = new Spookfox());

  browser.tabs.onCreated.addListener(async (openedTab) => {
    sf.dispatch(Actions.SAVE_TAB_START, openedTab.id);

    try {
      const savedTab = (await sf.request(
        EmacsRequests.TOGGLE_TAB_CHAINING,
        fromBrowserTab(openedTab)
      )) as SFTab;

      sf.dispatch(Actions.SAVE_TAB_SUCCESS, { savedTab, openedTab: openedTab });
    } catch (error) {
      console.error('Failed TOGGLE_TAB_CHAINING [err=', error, ']');
      sf.dispatch(Actions.SAVE_TAB_FAIL, { openedTab, error });
    }
  });

  browser.tabs.onUpdated.addListener(async (tabId, patch) => {
    // We aren't interested in all the tab changes (e.g which tab is active).
    // Here is a list of properties which we care about
    const desiredProps = ['url', 'title'];
    const changedDesiredProps = Object.keys(patch).filter((k) =>
      desiredProps.includes(k)
    );
    if (!changedDesiredProps.length) {
      return;
    }

    const tab = sf.state.openTabs[`${tabId}`];
    const savedTab = sf.state.savedTabs[tab.savedTabId];

    sf.dispatch(Actions.UPDATE_TAB_START, { tabId, patch });

    if (savedTab?.chained) {
      try {
        const updatedTab = await sf.request('UPDATE_TAB', {
          id: savedTab.id,
          patch,
        });
        sf.dispatch(Actions.UPDATE_TAB_SUCCESS, updatedTab);
      } catch (error) {
        console.error(
          `Error during updating tabs. [tabId=${tabId}, error=${error}]`
        );
        sf.dispatch(Actions.UPDATE_TAB_FAIL, { error, tabId });
      }
    }
  });

  browser.tabs.onRemoved.addListener(async (tabId, { windowId }) => {
    if (windowId !== 1) return;
    if (sf.state.savingTabs.includes(tabId)) await sleep(600);

    const openedTab = sf.state.openTabs[`${tabId}`];
    const savedTab = sf.state.savedTabs[openedTab.savedTabId];
    sf.dispatch(Actions.REMOVE_TAB_START, { tabId });

    if (savedTab?.chained) {
      try {
        const removedTab = await sf.request(
          'REMOVE_TAB',
          fromBrowserTab(openedTab)
        );
        sf.dispatch(Actions.REMOVE_TAB_SUCCESS, removedTab);
      } catch (error) {
        console.error(
          `Error during removing tab. [tabId=${tabId}, error=${error}]`
        );
        sf.dispatch(Actions.REMOVE_TAB_FAIL, { tabId, error });
      }
    }
  });

  // Ensure page-action icons (chained icon) for all tabs are always correct
  sf.addEventListener(SFEvents.NEW_STATE, async (e: SFEvent<State>) => {
    const state = e.payload;
    const tabs = Object.values(state.openTabs);
    const iconColor = window.matchMedia('(prefers-color-scheme: dark)').matches
      ? 'light'
      : 'dark';

    tabs.forEach(async (tab) => {
      if (!tab || !tab.id) return;
      const savedTab = state.savedTabs[tab.savedTabId];
      let icon = `icons/unchained-${iconColor}.svg`;

      if (savedTab && savedTab.chained) {
        icon = `icons/chained-${iconColor}.svg`;
      }

      try {
        await browser.pageAction.setIcon({ tabId: tab.id, path: icon });
        browser.pageAction.show(tab.id);
      } catch (err) {
        if (/invalid tab id/.test(err.message.toLowerCase())) {
          // pass. Tab has been deleted somehow, e.g Firefox containers do this
          // rapid tab open/close dance
          return;
        }

        console.warn(
          `Error occurred while setting pageAction icon. [err=${err}]`
        );
      }
    });
  });

  browser.pageAction.onClicked.addListener(async (t) => {
    const state = { ...sf.state };
    const openedTab = state.openTabs[t.id];
    const localSavedTab = state.savedTabs[openedTab.savedTabId] || {};
    sf.dispatch(Actions.SAVE_TAB_START, openedTab.id);

    try {
      const savedTab = (await sf.request(
        'TOGGLE_TAB_CHAINING',
        fromBrowserTab({
          ...localSavedTab,
          ...openedTab,
        })
      )) as SFTab;
      sf.dispatch(Actions.SAVE_TAB_SUCCESS, { savedTab, openedTab });
    } catch (error) {
      console.error(`Error during page-action. [err=${error}]`);
      sf.dispatch(Actions.SAVE_TAB_FAIL, { openedTab, error });
    }
  });

  sf.registerReqHandler('GET_ACTIVE_TAB', getActiveTab);
  sf.registerReqHandler('GET_ALL_TABS', getAllTabs);
  sf.registerReqHandler('OPEN_TAB', openTab);
  sf.registerReqHandler('OPEN_TABS', openTabs);
  sf.registerReqHandler('SEARCH_FOR', searchFor);
};

run().catch((err) => {
  console.error('An error occurred in run()', err);
});
