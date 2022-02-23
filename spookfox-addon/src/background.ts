interface SFMessage {
  type: 'Success' | 'Error';
  payload: string;
  sender: string;
}

interface SFAction {
  type: string;
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

const fromBrowserTab = (tab: browser.tabs.Tab): Tab => {
  return {
    id: tab.id,
    title: tab.title,
    url: tab.url,
    isPinned: tab.pinned,
  };
};

const port = browser.runtime.connectNative('spookfox');

port.onDisconnect.addListener((p) => {
  console.log('Disconnected from Native APP');
  if (p.error) {
    console.error('Disconnected due to error', p.error);
  }
});

port.onMessage.addListener(async (msg: SFMessage) => {
  if (!msg.payload) {
    console.warn('Unknown message:', msg);
    return;
  }

  try {
    const action: SFAction = JSON.parse(msg.payload);

    switch (action.type) {
      case 'GET_ACTIVE_TAB': {
        const msg = await getActiveTab();
        return port.postMessage(msg);
      }
      case 'GET_ALL_TABS': {
        const msg = await getAllTabs();
        return port.postMessage(msg);
      }
      default:
        console.warn(`Unknown action [action=${JSON.stringify(action)}]`);
    }
  } catch (err) {
    console.error(`Bad message payload [err=${err}, msg=${msg.payload}]`);
  }
});

const getActiveTab = async (): Promise<Result<Tab>> => {
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

browser.browserAction.onClicked.addListener(async () => {
  console.warn('TABS', await getAllTabs());
});
