import { Spookfox } from './Spookfox';

/**
 * A tab saved in Emacs.
 * We need to maintain our own mapping of which tab is saved where in
 * Emacs because browser don't provide a way to uniquely identify tabs.
 * `browser.tabs.Tab.id` is ephemeral, and gets reset every time browser
 * window is reopened.
 */
export interface SFTab {
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
export interface OpenTab extends browser.tabs.Tab {
  savedTabId?: string;
}

/**
 * A convenient function to convert an `OpenTab` to `SFTab` for when we need
 * to communicate with Emacs.
 */
export const fromBrowserTab = (tab: OpenTab): SFTab => {
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

/**
 * Get the active tab of first browser window. Let's not go in the nuances of
 * multiple browser windows just yet. For now, we work with the first browser
 * window that was opened. In Firefox, this window gets an id=1. Might need to
 * check this behavior when we are adding chrome support.
 */
export const getActiveTab = async (): Promise<SFTab> => {
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

/**
 * Get all tabs which are open in first browser window. Follows same semantics
 * as `getActiveTab`
 */
export const getAllTabs = async (): Promise<SFTab[]> => {
  const tabs = await browser.tabs.query({ windowId: 1 });

  return tabs.map(fromBrowserTab);
};

export const openTab = async (
  p: {
    id?: string;
    url: string;
  },
  sf: Spookfox
): Promise<SFTab> => {
  const state = { ...sf.state };
  if (p.id) {
    const tab = Object.values(state.openTabs).find(
      (t) => t?.savedTabId === p.id
    );

    if (tab) {
      await browser.tabs.update(tab.id, { active: true });

      return fromBrowserTab(tab);
    } else {
      // FIXME
      // A hackish way to ensure we don't create duplicate entries in org file
      // when a tab is opened from Emacs. Ideally, we should keep the original
      // entry and not create a new entry at all; but since we listen to
      // browser.tabs.onCreated event, as soon as we create a new tab, it is
      // saved in org-file.
      await sf.request('REMOVE_TAB', { id: p.id });
    }
  }

  const tab = await browser.tabs.create({ url: p.url });

  return fromBrowserTab(tab);
};

export const openTabs = async (
  tabs: {
    id: string;
    url: string;
  }[],
  sf: Spookfox
): Promise<SFTab[]> => {
  const openedTabs = await Promise.all(tabs.map((t) => openTab(t, sf)));

  return openedTabs;
};
