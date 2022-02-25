interface SFMessage {
  type: 'Success' | 'Error';
  payload: string;
  sender: string;
}

interface OpenTabActionPayload {
  url?: string;
  tabId?: string; // It should be a number, but JSON.parse(payload) don't convert it. Let's go with it being a string for now.
  tags?: string[];
}

type SearchActionPayload = string;

type OpenTabsActionPayload = OpenTabActionPayload[];

type ActionPayload =
  | OpenTabActionPayload
  | SearchActionPayload
  | OpenTabsActionPayload;

interface SFAction {
  type: string;
  payload: ActionPayload;
}

interface Tab {
  id: number;
  title: string;
  url: string;
  isPinned: boolean;
}

interface SFErrorMessage {
  type: 'Error';
  message: string;
}

type Result<R> = SFErrorMessage | R;
type ActionResponse = object;
type ActionResult = Result<ActionResponse>;

const fromBrowserTab = (tab: browser.tabs.Tab): Tab => {
  return {
    id: tab.id,
    title: tab.title,
    url: tab.url,
    isPinned: tab.pinned,
  };
};

const getActiveTab = async (): Promise<ActionResult> => {
  const tabs = await browser.tabs.query({ currentWindow: true, active: true });
  if (!tabs.length) {
    return { type: 'Error', message: 'No active tabs found' };
  }

  return fromBrowserTab(tabs[0]);
};

const getAllTabs = async () => {
  const tabs = await browser.tabs.query({ currentWindow: true });

  return tabs.map(fromBrowserTab);
};

const openTab = async (p: OpenTabActionPayload) => {
  let tab = null;
  try {
    tab = p.tabId && (await browser.tabs.get(parseInt(p.tabId, 10)));
  } catch (err) {
    // pass
  }

  if (tab) {
    browser.tabs.update(tab.id, { active: true });
  } else {
    browser.tabs.create({ url: p.url });
  }

  return {};
};

const openTabs = async (tabs: OpenTabsActionPayload) => {
  tabs.forEach(openTab);

  return {};
};

const searchFor = async (p: string) => {
  browser.search.search({ query: p });

  return {};
};

const actionsRepo: {
  [actionType: string]: (payload: ActionPayload) => Promise<ActionResult>;
} = {
  GET_ACTIVE_TAB: getActiveTab,
  GET_ALL_TABS: getAllTabs,
  OPEN_TAB: openTab,
  OPEN_TABS: openTabs,
  SEARCH_FOR: searchFor,
};

const init = () => {
  const port = browser.runtime.connectNative('spookfox');

  port.onDisconnect.addListener((p) => {
    console.log('Disconnected from Native APP');
    if (p.error) {
      console.error('Disconnected due to error', p.error);
    }
  });

  port.onMessage.addListener(async (msg: SFMessage) => {
    if (msg.type === 'Error') {
      console.error('spookfox-native faced an error, [err=', msg.payload, ']');
      return;
    }

    if (!msg.payload) {
      console.warn('Unknown message:', msg);
      return;
    }

    try {
      const action: SFAction = JSON.parse(msg.payload);
      const executioner = actionsRepo[action.type];

      if (executioner) {
        const msg = await executioner(action.payload);
        return port.postMessage(msg);
      } else {
        console.warn(`Unknown action [action=${JSON.stringify(action)}]`);
      }
    } catch (err) {
      console.error(`Bad message payload [err=${err}, msg=${msg.payload}]`);
    }
  });

  browser.browserAction.onClicked.addListener(async () => {
    console.warn('TABS', await getAllTabs());
  });
};

init();
