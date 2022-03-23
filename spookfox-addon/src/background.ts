import OrgTabs, {
  Actions,
  fromBrowserTab,
  OrgTabsState,
  SavedTab,
} from './apps/OrgTabs';
import { Spookfox, SFEvent, SFEvents } from './Spookfox';

const run = async () => {
  const sf = ((window as any).sf = new Spookfox());

  sf.registerApp('orgTabs', OrgTabs);

  // Ensure page-action icons (chained icon) for all tabs are always correct
  sf.addEventListener(SFEvents.NEW_STATE, async (e: SFEvent) => {
    const tabs = await browser.tabs.query({ windowId: 1 });
    const iconColor = window.matchMedia('(prefers-color-scheme: dark)').matches
      ? 'light'
      : 'dark';
    const state: OrgTabsState = e.payload.orgTabs;

    tabs.forEach(async (tab) => {
      const openTab = state.openTabs[tab.id];
      const savedTab = state.savedTabs[openTab?.savedTabId];
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
    const state = (sf.state as any).orgTabs as OrgTabsState;
    const openedTab = state.openTabs[t.id];
    const localSavedTab = state.savedTabs[openedTab.savedTabId] || {};
    const app = sf.apps.orgTabs as OrgTabs;
    app.dispatch(Actions.SAVE_TAB_START, openedTab.browserTabId);

    try {
      const savedTab = (await sf.request(
        'TOGGLE_TAB_CHAINING',
        fromBrowserTab({
          ...localSavedTab,
          ...openedTab,
        })
      )) as SavedTab;
      // FIXME This should be a part of OrgTabs app
      (sf.apps.orgTabs as OrgTabs).dispatch(Actions.SAVE_TAB_SUCCESS, {
        savedTab,
        openedTab,
      });
    } catch (error) {
      console.error(`Error during page-action. [err=${error}]`);
      (sf.apps.orgTabs as OrgTabs).dispatch(Actions.SAVE_TAB_FAIL, {
        openedTab,
        error,
      });
    }
  });
};

run().catch((err) => {
  console.error('An error occurred in run()', err);
});
