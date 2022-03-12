import { sleep } from './lib';
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

  browser.tabs.onCreated.addListener(async (tab) => {
    sf.newState(
      {
        ...sf.state,
        openTabs: { ...sf.state.openTabs, [`${tab.id}`]: tab },
        savingTabs: sf.state.savingTabs.concat([tab.id]),
      },
      `SAVING_TAB ${tab.id}`
    );

    const savedTab = (await sf.request(
      'TOGGLE_TAB_CHAINING',
      fromBrowserTab(tab)
    )) as SFTab;
    const savedTabs = { ...sf.state.savedTabs };
    const openTabs = { ...sf.state.openTabs };
    savedTabs[savedTab.id] = savedTab;
    openTabs[`${tab.id}`] = { ...tab, savedTabId: savedTab.id };

    sf.newState(
      {
        ...sf.state,
        savingTabs: sf.state.savingTabs.filter((id) => id !== tab.id),
        savedTabs,
        openTabs,
      },
      `SAVED_TAB ${tab.id}`
    );
  });

  browser.tabs.onUpdated.addListener(async (tabId, changeInfo) => {
    // We aren't interested in all the tab changes (e.g which tab is active).
    // Here is a list of properties which we care about
    const desiredProps = ['url', 'title'];
    const changedDesiredProps = Object.keys(changeInfo).filter((k) =>
      desiredProps.includes(k)
    );
    if (!changedDesiredProps.length) {
      return;
    }

    const tab = sf.state.openTabs[`${tabId}`];
    const savedTab = sf.state.savedTabs[tab.savedTabId];
    const updatedTab = { ...tab, ...changeInfo };

    if (savedTab?.chained) {
      sf.request(
        'UPDATE_TAB',
        // augment with savedTab for Emacs side properties like 'chained'
        fromBrowserTab({ ...savedTab, ...updatedTab } as OpenTab)
      ).catch((err) => {
        console.warn(
          `Error during updating tabs. [tabId=${tabId}, err=${err}]`
        );
      });
    }

    const openTabs = { ...sf.state.openTabs };
    openTabs[`${tabId}`] = updatedTab;

    sf.newState({ ...sf.state, openTabs }, `UPDATED_TAB ${tabId}`);
  });

  // For debugging state transitions
  sf.addEventListener(SFEvents.NEW_STATE, (e: SFEvent) => {
    if (!localStorage.getItem('DEBUG_NEW_STATE')) return;
    console.groupCollapsed(`${e.name} ${e.debugMessage || ''}`);
    console.log(e.payload);
    console.groupEnd();
  });

  browser.tabs.onRemoved.addListener(async (tabId, { windowId }) => {
    if (windowId !== 1) return;
    if (sf.state.savingTabs.includes(tabId)) await sleep(600);

    const tab = sf.state.openTabs[`${tabId}`];
    const savedTab = sf.state.savedTabs[tab.savedTabId];

    if (savedTab?.chained) {
      await sf.request('REMOVE_TAB', fromBrowserTab(tab));
    }
    const openTabs = { ...sf.state.openTabs };
    delete openTabs[`${tabId}`];

    sf.newState({ ...sf.state, openTabs }, `REMOVED_TAB ${tabId}`);
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

    sf.newState(state, 'CLICKED_PAGE_ACTION');
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
