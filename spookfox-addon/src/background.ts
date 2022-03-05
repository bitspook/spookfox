import { v4 as uuid } from 'uuid';
import { Spookfox, SFEvent, SFEvents, State } from './Spookfox';
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

  browser.tabs.onCreated.addListener((tab) => {
    const state = { ...sf.state };
    state.openTabs[`${tab.id}`] = tab;

    sf.newState(state);
  });

  browser.tabs.onUpdated.addListener((tabId, changeInfo) => {
    // We aren't interested in all the tab changes (e.g which tab is active).
    // Here is a list of properties which we care about
    const desiredProps = ['url'];
    const changedDesiredProps = Object.keys(changeInfo).filter((k) =>
      desiredProps.includes(k)
    );
    if (!changedDesiredProps.length) {
      return;
    }

    const state = { ...sf.state };
    const tab = state.openTabs[`${tabId}`];
    state.openTabs[`${tabId}`] = { ...tab, ...changeInfo };

    sf.newState(state);
  });

  browser.tabs.onRemoved.addListener((tabId, { windowId }) => {
    if (windowId !== 1) return;
    const state = { ...sf.state };
    state.openTabs[`${tabId}`] = null;

    sf.newState(state);
  });

  // For debugging
  sf.addEventListener(SFEvents.NEW_STATE, (e: SFEvent<State>) => {
    console.warn('NEW_STATE', e.payload);
  });

  // Ensure page-action icons (chained icon) for all tabs are always correct
  sf.addEventListener(SFEvents.NEW_STATE, (e: SFEvent<State>) => {
    const state = e.payload;
    const tabs = Object.values(state.openTabs);
    const iconColor = window.matchMedia('(prefers-color-scheme: dark)').matches
      ? 'light'
      : 'dark';

    tabs.forEach((tab) => {
      if (!tab) return;
      const savedTab = state.savedTabs[tab.savedTabId];
      let icon = `icons/unchained-${iconColor}.svg`;

      if (savedTab && savedTab.chained) {
        icon = `icons/chained-${iconColor}.svg`;
      }

      browser.pageAction.setIcon({
        tabId: tab.id,
        path: icon,
      });
    });
  });

  browser.pageAction.onClicked.addListener(async (t) => {
    const state = { ...sf.state };
    const tab = state.openTabs[t.id];
    const localSavedTab = state.savedTabs[tab.savedTabId] || {};

    const savedTab = (await sf.request(
      'TOGGLE_TAB_CHAINING',
      fromBrowserTab({
        ...localSavedTab,
        ...tab,
      })
    )) as SFTab;

    state.openTabs[`${tab.id}`].savedTabId = savedTab.id;
    state.savedTabs[savedTab.id] = savedTab;

    sf.newState(state);
  });

  browser.browserAction.onClicked.addListener(async () => {
    const savedTabs = await sf.request('GET_SAVED_TABS');

    console.warn('SAVED TABS', savedTabs);
  });

  sf.registerReqHandler('GET_ACTIVE_TAB', getActiveTab);
  sf.registerReqHandler('GET_ALL_TABS', getAllTabs);
  sf.registerReqHandler('OPEN_TAB', openTab);
  sf.registerReqHandler('OPEN_TABS', openTabs);
  sf.registerReqHandler('SEARCH_FOR', searchFor);
};

run();
