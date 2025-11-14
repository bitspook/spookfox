/**
 * Events known to Spookfox.
 * For documentation and ease of refactoring.
 */
const SFEvents = {
  CONNECTED: 'CONNECTED',
  CONNECTING: 'CONNECTING',
  DISCONNECTED: 'DISCONNECTED',
  // A request Emacs sent to do something or to provide some information
  REQUEST: 'REQUEST',
  // Response Emacs sent for a request we made
  RESPONSE: 'RESPONSE'
};

const LogLevel = {
  Error: 0,
  Info: 1,
  Debug: 2,
};

/**
 * `Spookfox` is the heart of this addon.
 *
 * # Usage
 *
 *   ```js
 *   const sf = new Spookfox();
 *   ```
 *
 * # Events
 *
 * It emits `SFEvents`. `SFEvents.REQUEST` and `SFEvents.RESPONSE` don't
 * need to be handled manually. `Spookfox.request` and `Spookfox.registerReqHandler`
 * should be sufficient for most cases.
 */
// Extends `EventTarget` so we can have the ability to emit and listen to custom
// events. We rely on custom events to build independent modules, while
// providing a unified interface.
class Spookfox extends EventTarget {
  reqHandlers = {};
  // This is needed only for re-init hack
  eventListeners = [];
  wsUrl = 'ws://localhost:59001';
  ws = null;
  reConnecting = false;

  constructor() {
    super();
    this.ws = this.reConnect();
    this.setupEventListeners();
  }

  get isConnected() {
    return this.ws && this.ws.readyState === this.ws.OPEN;
  }

  get logLevel() {
    return parseInt(localStorage.getItem('DEBUG'), 10) || LogLevel.Info;
  }

  log(level, ...msg) {
    if (this.logLevel >= level) {
      console.log(...msg);
    }
  }

  addEventListener(type, callback) {
    this.eventListeners.push({ type, callback });
    super.addEventListener(type, callback);
  }

  removeEventListener(type, callback) {
    this.eventListeners = this.eventListeners.filter(
      (el) => !(el.type === type && el.callback === callback)
    );
    super.removeEventListener(type, callback);
  }

  setupEventListeners() {
    this.addEventListener(SFEvents.REQUEST, this.handleRequest);
    this.addEventListener(SFEvents.RESPONSE, this.handleResponse);
  }

  handleServerMsg = async (event) => {
    try {
      const msg = JSON.parse(event.data);

      this.log(LogLevel.Debug, 'ws message:', event);

      if (msg.name) {
        return this.emit(SFEvents.REQUEST, msg);
      }

      return this.emit(SFEvents.RESPONSE, msg);
    } catch (err) {
      console.error(`Bad ws message [err=${err}, msg=${event.data}]`);
    }
  };

  reConnect() {
    if (this.ws) {
      this.ws.close();
      this.reConnecting = true;
    }

    if (!this.reConnecting) this.emit(SFEvents.CONNECTING);
    this.ws = new WebSocket(this.wsUrl);

    const handleWsOpen = () => {
      this.emit(SFEvents.CONNECTED);
    };

    const handleWsClose = () => {
      this.ws.removeEventListener('open', handleWsOpen);
      this.ws.removeEventListener('close', handleWsClose);
      this.ws.removeEventListener('message', this.handleServerMsg);

      this.emit(SFEvents.DISCONNECTED);
      if (this.reConnecting) this.emit(SFEvents.CONNECTING);

      this.ws = null;
      this.reConnecting = false;
    };

    this.ws.addEventListener('open', handleWsOpen);
    this.ws.addEventListener('close', handleWsClose);
    this.ws.addEventListener('message', this.handleServerMsg);

    return this.ws;
  }

  /**
   * Send a request with NAME and PAYLOAD to Emacs.
   * Returns a promise of response returned by Emacs.
   * # Example
   * ```
   * const savedTabs = sf.request('GET_SAVED_TABS');
   * ```
   */
  async request(name, payload) {
    if (!this.ws) {
      console.error(
        `Not connected to Spookfox Server. Dropping request ${name}.`
      );
      return;
    }

    const request = {
      id: crypto.randomUUID(),
      name,
      payload,
    };

    this.ws.send(JSON.stringify(request));

    const res = await this.getResponse(request.id);
    if (res.payload.status?.toLowerCase() === 'error') {
      throw new Error(res.payload.message);
    }

    return res.payload;
  }

  /**
   * A convenience function for emitting new events to `Spookfox`.
   */
  emit = (name, payload) => {
    const event = new Event(name);
    event.payload = payload;

    this.log(LogLevel.Debug, 'Emitting', event);

    this.dispatchEvent(event);
  };

  /**
   * Run a function when Emacs makes a request.
   * # Example
   * ```
   * sf.registerReqHandler('OPEN_TAB', ({ url }) => {
   *   // somehow open a new tab with `url` provided by Emacs.
   * })
   * ```
   */
  registerReqHandler(name, handler) {
    if (this.reqHandlers[name] && this.logLevel) {
      this.log(LogLevel.Info, `Overwriting handler for: ${name}`);
    }

    this.reqHandlers[name.toUpperCase()] = handler;
  }

  /**
   * Handle `SFEvents.REQUEST` events.
   */
  handleRequest = async (e) => {
    const request = e.payload;

    this.log(LogLevel.Debug, 'Received request:', e);

    const executioner = this.reqHandlers[request.name.toUpperCase()];

    if (!executioner) {
      console.warn('No handler for request', { request });
      return;
    }
    const response = await executioner(request.payload, this);

    return this.ws?.send(
      JSON.stringify({
        requestId: request.id,
        payload: response,
      })
    );
  };

  /**
   * Handle `SFEvents.RESPONSE` events.
   */
  handleResponse = async (e) => {
    const res = e.payload;

    if (!res.requestId) {
      throw new Error(`Invalid response: [res=${JSON.stringify(res)}]`);
    }

    // Emit a unique event per `requestId`. Shenanigans I opted for doing
    // to build a promise based interface on request/response dance needed
    // for communication with Emacs. Check `Spookfox.getResponse`
    this.emit(res.requestId, res);
  };

  getResponse = (requestId) => {
    const maxWait = 5000;

    return new Promise((resolve, reject) => {
      const listener = (event) => {
        clearTimeout(killTimer);
        this.removeEventListener(requestId, listener);

        resolve(event.payload);
      };
      this.addEventListener(requestId, listener);

      // If it's taking too long to for Emacs to respond, something bad has
      // probably happened and we aren't getting any response. Time to abort the
      // response.
      const killTimer = setTimeout(() => {
        this.removeEventListener(requestId, listener);
        reject(new Error('Spookfox response timeout.'));
      }, maxWait);
    });
  };
}

const iconEmacsMono = './icons/emacs-mono.svg';
const iconEmacsColor = './icons/emacs-color.svg';

let autoConnectInterval = null;
let connectedPorts = [];

// Messages from content script
browser.runtime.onMessage.addListener((msg) => {
  const sf = window.spookfox;
  switch (msg.type) {
    case 'SPOOKFOX_RELAY_TO_EMACS': {
      sf.request(msg.action.name, msg.action.payload);
    }
  }
});

// Messages from popup
browser.runtime.onConnect.addListener((port) => {
  connectedPorts.push(port);
  port.postMessage({
    type: window.spookfox.isConnected ? 'CONNECTED' : 'DISCONNECTED',
  });

  port.onDisconnect.addListener(
    () => (connectedPorts = connectedPorts.filter((p) => p !== port))
  );

  port.onMessage.addListener((msg) => {
    const sf = window.spookfox;

    switch (msg.type) {
      case 'RECONNECT':
        return sf.reConnect();
    }
  });
});

const startAutoconnectTimer = (sf) => {
  sf.addEventListener(SFEvents.CONNECTED, () => {
    browser.browserAction.setIcon({ path: iconEmacsColor });

    if (autoConnectInterval) clearInterval(autoConnectInterval);

    connectedPorts.forEach((port) => {
      port.postMessage({ type: 'CONNECTED' });
    });
  });

  sf.addEventListener(SFEvents.CONNECTING, () => {
    connectedPorts.forEach((port) => {
      port.postMessage({ type: 'CONNECTING' });
    });
  });

  sf.addEventListener(SFEvents.DISCONNECTED, () => {
    connectedPorts.forEach((port) => {
      port.postMessage({ type: 'DISCONNECTED' });
    });

    browser.browserAction.setIcon({ path: iconEmacsMono });
    if (!autoConnectInterval) {
      autoConnectInterval = setInterval(() => {
        sf.reConnect();
      }, 5000);
    }
  });
};

const addJsInjection = (sf) => {
  const EmacsRequests = {
    EVAL_IN_ACTIVE_TAB: 'JS_INJECT_EVAL_IN_ACTIVE_TAB',
    EVAL_IN_BACKGROUND_SCRIPT: 'JS_INJECT_EVAL_IN_BACKGROUND_SCRIPT',
    EVAL_IN_TAB: 'JS_INJECT_EVAL_IN_TAB',
  };

  const evalJsInActiveTab = async (script) => {
    const currentWindow = await browser.windows.getCurrent();
    const activeTabs = await browser.tabs.query({
      active: true,
      windowId: currentWindow.id,
    });

    if (!activeTabs.length) {
      throw new Error(
        'No active tab to execute script in. [script=${JSON.stringify(script)}]'
      );
    }

    return Promise.all(
      activeTabs.map((tab) => browser.tabs.executeScript(tab.id, script))
    );
  };

  evalJsInBackgroundScript = async ({ code }) => {
    const result = window.eval(code);

    return result;
  };

  evalJsInTab = async ({ code, 'tab-id': tabId }) => {
    return browser.tabs.executeScript(tabId, { code });
  };

  sf.registerReqHandler(EmacsRequests.EVAL_IN_ACTIVE_TAB, evalJsInActiveTab);
  sf.registerReqHandler(
    EmacsRequests.EVAL_IN_BACKGROUND_SCRIPT,
    evalJsInBackgroundScript
  );
  sf.registerReqHandler(EmacsRequests.EVAL_IN_TAB, evalJsInTab);
};

const addTabManipulation = (sf) => {
  const EmacsRequests = {
    GET_ACTIVE_TAB: 'GET_ACTIVE_TAB',
    GET_ALL_TABS: 'GET_ALL_TABS',
    OPEN_TAB: 'OPEN_TAB',
    FOCUS_TAB: 'FOCUS_TAB',
    SEARCH_FOR: 'SEARCH_FOR',
  };

  const serializeTab = (tab) => ({
    id: tab.id,
    url: tab.url,
    title: tab.title,
    windowId: tab.windowId,
  });

  const currentWindowId = async () => (await browser.windows.getCurrent()).id;

  /**
   * Get the active tab of given browser-window, or if none provided, of current
   * browser window. Current-browser window is decided by browser.
   */
  const getActiveTab = async (msg) => {
    const windowId = (msg || {}).windowId || (await currentWindowId());
    const tabs = await browser.tabs.query({ windowId, active: true });

    if (!tabs.length) {
      // Probably shouldn't be doing this, but just throwing an error and calling it
      // a day simplifies the types a lot. Besides I am not sure if there will ever
      // be a case when a window don't have an active tab. This check is here because
      // doing `tabs[0]` give me heebie-jeebies
      throw new Error('No active tab found');
    }

    return tabs[0];
  };

  const openSearchTab = async (p) => {
    browser.search.search({ query: p });

    return {};
  };

  const getAllTabs = async (msg = {}) => {
    const windowId = (msg || {}).windowId;
    let tabs = [];
    if (windowId) {
      tabs = await browser.tabs.query({ windowId });
    } else {
      const allWindows = await browser.windows.getAll();
      tabs = (
        await Promise.all(
          allWindows.map(({ id }) => {
            return browser.tabs.query({ windowId: id });
          })
        )
      ).flat();
    }

    return tabs.map(serializeTab);
  };

  const openTab = async (p) => await browser.tabs.create({ url: p.url });

  const focusTab = async ({ 'tab-id': tabId, 'window-id': windowId }) => {
    await browser.tabs.update(tabId, { active: true });

    if (windowId) await browser.windows.update(windowId, { focused: true });
  };

  sf.registerReqHandler(EmacsRequests.GET_ACTIVE_TAB, getActiveTab);
  sf.registerReqHandler(EmacsRequests.GET_ALL_TABS, getAllTabs);
  sf.registerReqHandler(EmacsRequests.OPEN_TAB, openTab);
  sf.registerReqHandler(EmacsRequests.FOCUS_TAB, focusTab);
  sf.registerReqHandler(EmacsRequests.SEARCH_FOR, openSearchTab);
};

const run = async () => {
  const sf = (window.spookfox = new Spookfox());

  addJsInjection(sf);
  addTabManipulation(sf);

  startAutoconnectTimer(sf);
};

run().catch((err) => {
  console.error('An error occurred in run()', err);
});
