import { v4 as uuid } from 'uuid';

interface Packet {
  status: 'Success' | 'Error';
  message: string;
  sender: string;
}

interface OpenTabActionPayload {
  url?: string;
  tab_id?: string; // It should be a number, but JSON.parse(payload) don't convert it. Let's go with it being a string for now.
  tags?: string[];
}

type SearchActionPayload = string;

type OpenTabsActionPayload = OpenTabActionPayload[];

type ActionPayload =
  | OpenTabActionPayload
  | SearchActionPayload
  | OpenTabsActionPayload;

type Message = Action | ActionResponse;

interface ActionResponse {
  actionId: string;
  payload: any;
}

interface Action {
  id: string;
  action: string;
  payload: ActionPayload;
}

interface Tab {
  id?: string; // id exists iff tab is saved in Emacs
  tab_id: number;
  title: string;
  url: string;
  is_pinned: boolean;
}

interface SFErrorMessage {
  type: 'Error';
  message: string;
}

type Result<R> = SFErrorMessage | R;

const fromBrowserTab = (tab: browser.tabs.Tab): Tab => {
  return {
    tab_id: tab.id,
    title: tab.title,
    url: tab.url,
    is_pinned: tab.pinned,
  };
};

const getActiveTab = async (): Promise<Result<Tab>> => {
  const tabs = await browser.tabs.query({ currentWindow: true, active: true });
  if (!tabs.length) {
    return { type: 'Error', message: 'No active tabs found' };
  }

  return fromBrowserTab(tabs[0]);
};

const getAllTabs = async (): Promise<Tab[]> => {
  const tabs = await browser.tabs.query({ currentWindow: true });

  return tabs.map(fromBrowserTab);
};

const openTab = async (p: OpenTabActionPayload): Promise<Tab> => {
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

const openTabs = async (tabs: OpenTabsActionPayload): Promise<Tab[]> => {
  const openedTabs = await Promise.all(tabs.map(openTab));

  return openedTabs;
};

const searchFor = async (p: string) => {
  browser.search.search({ query: p });

  return {};
};

const actionsRepo: {
  [actionType: string]: (payload: ActionPayload) => Promise<Result<any>>;
} = {
  GET_ACTIVE_TAB: getActiveTab,
  GET_ALL_TABS: getAllTabs,
  OPEN_TAB: openTab,
  OPEN_TABS: openTabs,
  SEARCH_FOR: searchFor,
};

// Let's centralize all our communication
class SocketMan extends EventTarget {
  private responses: { [actionId: string]: ActionResponse } = {};
  port: browser.runtime.Port;

  constructor(port: browser.runtime.Port) {
    super();
    this.port = port;
  }

  sendAction(name: string, payload?: ActionPayload) {
    const action: Action = {
      id: uuid(),
      action: name,
      payload,
    };

    this.port.postMessage(action);

    return this.getResponse(action.id);
  }

  processAction = async (
    action: Action,
    port: browser.runtime.Port = (window as any).port
  ) => {
    const executioner = actionsRepo[action.action];

    if (!executioner) {
      console.warn(`Unknown action [action=${JSON.stringify(action)}]`);
    }

    const payload = await executioner(action.payload);
    return port.postMessage({
      actionId: action.id,
      payload,
    });
  };

  processResponse(res: ActionResponse) {
    this.responses[res.actionId] = res;
    this.dispatchEvent(new Event(res.actionId));
  }

  getResponse(actionId: string) {
    return new Promise((resolve) => {
      const listener = () => {
        const res = this.responses[actionId];
        this.responses[actionId] = null;
        this.removeEventListener(actionId, listener);

        resolve(res.payload);
      };

      this.addEventListener(actionId, listener);
    });
  }
}

const init = () => {
  const port = ((window as any).port =
    browser.runtime.connectNative('spookfox'));
  const socket = new SocketMan(port);

  port.onDisconnect.addListener((p) => {
    console.log('Disconnected from Native APP');
    if (p.error) {
      console.error('Disconnected due to error', p.error);
    }
  });

  browser.runtime.onConnect.addListener((p) => {});

  port.onMessage.addListener(async (pkt: Packet) => {
    if (pkt.status === 'Error') {
      console.error('spookfox-native faced an error, [err=', pkt.message, ']');
      return;
    }

    if (!pkt.message) {
      console.warn('Unknown message:', pkt);
      return;
    }

    try {
      const msg: Message = JSON.parse(pkt.message);

      if ((msg as Action).action) {
        return socket.processAction(msg as Action);
      }

      return socket.processResponse(msg as ActionResponse);
    } catch (err) {
      console.error(`Bad message payload [err=${err}, msg=${pkt.message}]`);
    }
  });

  browser.pageAction.onClicked.addListener(async (tab) => {
    const res = await socket.sendAction('CHAIN_TAB', fromBrowserTab(tab));
    console.warn('RESPONSE', res);

    return browser.pageAction.setIcon({
      tabId: tab.id,
      path: 'icons/chained-light.svg',
    });
  });

  browser.browserAction.onClicked.addListener(async () => {
    const savedTabs = await socket.sendAction('GET_SAVED_TABS');

    console.warn('SAVED TABS', savedTabs);
  });
};

init();
