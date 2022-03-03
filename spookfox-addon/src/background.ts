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
type ActionResult = Result<any>;

const fromBrowserTab = (tab: browser.tabs.Tab): Tab => {
  return {
    tab_id: tab.id,
    title: tab.title,
    url: tab.url,
    is_pinned: tab.pinned,
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
    tab = p.tab_id && (await browser.tabs.get(parseInt(p.tab_id, 10)));
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

class ResponseHandler extends EventTarget {
  responses: { [actionId: string]: ActionResponse } = {};

  addNew(res: ActionResponse) {
    this.responses[res.actionId] = res;
    this.dispatchEvent(new Event(res.actionId));
  }

  waitFor(actionId: string) {
    return new Promise((resolve, reject) => {
      const listener = () => {
        const res = this.responses[actionId];
        this.responses[actionId] = null;
        this.removeEventListener(actionId, listener);

        try {
          resolve(JSON.parse(res.payload));
        } catch (err) {
          console.error('Bad response from Emacs. [res=', res, ']');
          reject(err);
        }
      };

      this.addEventListener(actionId, listener);
    });
  }
}

const init = () => {
  const port = browser.runtime.connectNative('spookfox');
  const responseHandler = new ResponseHandler();

  const sendAction = (name: string, payload?: ActionPayload) => {
    const action: Action = {
      id: uuid(),
      action: name,
      payload,
    };

    port.postMessage(action);

    return action.id;
  };

  const handleAction = async (action: Action) => {
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

  port.onDisconnect.addListener((p) => {
    console.log('Disconnected from Native APP');
    if (p.error) {
      console.error('Disconnected due to error', p.error);
    }
  });

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
        return handleAction(msg as Action);
      }

      return responseHandler.addNew(msg as ActionResponse);
    } catch (err) {
      console.error(`Bad message payload [err=${err}, msg=${pkt.message}]`);
    }
  });

  browser.pageAction.onClicked.addListener(async (tab) => {
    sendAction('CHAIN_TAB', fromBrowserTab(tab));

    return browser.pageAction.setIcon({
      tabId: tab.id,
      path: 'icons/chained-light.svg',
    });
  });

  browser.browserAction.onClicked.addListener(async () => {
    const actionId = sendAction('GET_SAVED_TABS');
    const savedTabs = await responseHandler.waitFor(actionId);

    console.warn('SAVED TABS', savedTabs);
  });
};

init();
