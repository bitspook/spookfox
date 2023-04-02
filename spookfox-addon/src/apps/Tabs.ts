import { Draft, Immutable } from 'immer';
import browser from 'webextension-polyfill';
import { SFApp, Spookfox } from '~src/Spookfox';

export type TabsState = Immutable<null>;

export default class Tabs implements SFApp<TabsState> {
  initialState: Immutable<TabsState> = null;

  get state(): TabsState {
    return this.sf.state[this.name];
  }

  dispatch(name: Actions, payload: unknown) {
    return this.sf.dispatch(`${this.name}/${name}`, payload);
  }

  constructor(public name: string, public sf: Spookfox) {
    sf.registerReqHandler(EmacsRequests.GET_ACTIVE_TAB, this.getActiveTab);
    sf.registerReqHandler(EmacsRequests.GET_ALL_TABS, this.getAllTabs);
    sf.registerReqHandler(EmacsRequests.OPEN_TAB, this.openTab);
    sf.registerReqHandler(EmacsRequests.SEARCH_FOR, this.openSearchTab);
  }

  async currentWindowId() {
    const currentWindow = await browser.windows.getCurrent();

    return currentWindow.id;
  }

  /**
   * Get the active tab of given browser-window, or if none provided, of current
   * browser window. Current-browser window is decided by browser.
   */
  getActiveTab = async (msg: { windowId?: number }): Promise<any> => {
    const windowId = (msg || {}).windowId || (await this.currentWindowId());
    const tabs = await browser.tabs.query({ windowId, active: true });

    if (!tabs.length) {
      // Probably shouldn't be doing this, but just throwing an error and calling it
      // a day simplifies the types a lot. Besides I am not sure if there will ever
      // be a case when a window don't have an active tab. This check is here because
      // doing `tabs[0]` give me heebie-jeebies
      throw new Error('No active tab found');
    }

    return tabs[0];
  };

  openSearchTab = async (p: string) => {
    (browser as any).search.search({ query: p });

    return {};
  };

  /**
   * Get all tabs which are open in given or current browser window. Follows
   * same semantics as `getActiveTab`
   */
  getAllTabs = async (msg: { windowId?: number } = {}): Promise<any[]> => {
    const windowId = (msg || {}).windowId || (await this.currentWindowId());
    const tabs = await browser.tabs.query({ windowId });

    return tabs;
  };

  openTab = async (p: { url: string }): Promise<any> => {
    const tab = await browser.tabs.create({ url: p.url });

    return tab;
  };

  openTabs = async (
    tabs: {
      id: string;
      url: string;
    }[]
  ): Promise<any[]> => {
    const openedTabs = await Promise.all(tabs.map(this.openTab));

    return openedTabs;
  };

  /**
   * Initialize the state.
   */
  init = async () => {
    this.dispatch(Actions.INIT, null);
  };

  reducer(_, state: Draft<TabsState>) {
    return state;
  }
}

export enum Actions {
  INIT = 'INIT',
}

export enum EmacsRequests {
  GET_ACTIVE_TAB = 'T_GET_ACTIVE_TAB',
  GET_ALL_TABS = 'T_GET_ALL_TABS',
  OPEN_TAB = 'T_OPEN_TAB',
  SEARCH_FOR = 'T_SEARCH_FOR',
}
