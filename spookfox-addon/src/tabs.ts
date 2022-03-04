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
    title: tab.title,
    url: tab.url,
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

export const openTab = async (p: {
  tab_id: string;
  url: string;
}): Promise<SFTab> => {
  let tab = null;
  try {
    tab = p.tab_id && (await browser.tabs.get(parseInt(p.tab_id, 10)));
  } catch (err) {
    // pass
  }

  if (tab) {
    browser.tabs.update(tab.id, { active: true });
  } else {
    tab = browser.tabs.create({ url: p.url });
  }

  return tab;
};

export const openTabs = async (
  tabs: {
    tab_id: string;
    url: string;
  }[]
): Promise<SFTab[]> => {
  const openedTabs = await Promise.all(tabs.map(openTab));

  return openedTabs;
};
