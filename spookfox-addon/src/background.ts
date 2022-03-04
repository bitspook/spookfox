import { v4 as uuid } from 'uuid';
import { Spookfox, SFEvent, SFEvents } from './Spookfox';
import {
  SFTab,
  fromBrowserTab,
  getActiveTab,
  getAllTabs,
  openTab,
  openTabs,
} from './tabs';

const searchFor = async (p: string) => {
  browser.search.search({ query: p });

  return {};
};

const handleReconnect = async (event: SFEvent) => {
  const sf = event.target as Spookfox;

  const savedTabs = (await sf.request('GET_SAVED_TABS')) as SFTab[];
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
    if (savedTab) {
      takenSavedTabIds.push(savedTab.id);
      accum[tab.id] = {
        savedTabId: savedTab.id,
        ...tab,
      };
    } else {
      accum[tab.id] = tab;
    }

    return accum;
  }, {});

  const savedTabsMap = savedTabs.reduce((accum, tab) => {
    accum[tab.id] = tab;
    return accum;
  }, {});

  const newState = {
    ...sf.state,
    openTabs,
    savedTabs: savedTabsMap,
  };

  sf.newState(newState);
};

const run = async () => {
  const sf = ((window as any).sf = new Spookfox());

  browser.pageAction.onClicked.addListener(async (tab) => {
    const state = { ...sf.state };
    const tabWithId = state.openTabs[tab.id] || tab;
    const savedTab = (await sf.request(
      'TOGGLE_TAB_CHAINING',
      fromBrowserTab(tabWithId)
    )) as SFTab;
    state.savedTabs[savedTab.id] = savedTab;
    sf.newState(state);

    if (savedTab.chained) {
      browser.pageAction.setIcon({
        tabId: tab.id,
        path: 'icons/chained-light.svg',
      });
    } else {
      browser.pageAction.setIcon({
        tabId: tab.id,
        path: 'icons/unchained-light.svg',
      });
    }
  });

  browser.browserAction.onClicked.addListener(async () => {
    const savedTabs = await sf.request('GET_SAVED_TABS');

    console.warn('SAVED TABS', savedTabs);
  });

  sf.addEventListener(SFEvents.EMACS_CONNECTED, handleReconnect);

  sf.registerHandler('GET_ACTIVE_TAB', getActiveTab);
  sf.registerHandler('GET_ALL_TABS', getAllTabs);
  sf.registerHandler('OPEN_TAB', openTab);
  sf.registerHandler('OPEN_TABS', openTabs);
  sf.registerHandler('SEARCH_FOR', searchFor);
};

run();
