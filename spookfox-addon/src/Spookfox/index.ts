import { v4 as uuid } from 'uuid';
import { OpenTab, SFTab } from '../tabs';
import setupBroker from './broker';

interface Response {
  requestId: string;
  payload: any;
}

interface Request {
  id: string;
  name: string;
  payload: any;
}

/**
 * Mutable data we need to keep track of. Lesser the better.
 */
export interface State {
  // All the tabs open in browser at any time. Assumption is that this list is
  // created when Emacs first connects, and then kept up-to-date by browser
  // itself whenever anything related to a tab changes
  openTabs: { [id: string]: OpenTab };
  // All tabs which are saved in Emacs. We obtain this list when Emacs first
  // connects. After that, any time something related to saved tabs changes,
  // this list should be updated *after the face*. i.e make the change in Emacs,
  // and then ask Emacs how this list looks like; either by asking for the whole
  // list again, or designing the response such that Emacs returns the updated
  // `SFTab`
  savedTabs: { [id: string]: SFTab };
  // Firefox containers, when configured to "auto-close tabs" cause a
  // race-condition where they rapidly create+close+create tabs. In this case,
  // the close callback gets called very quickly. On tab create, Emacs save the
  // tab and respond with the saved tab. But before it can do this, Firefox has
  // already closed the tab and emitted onClosed event. onClosed checks if we
  // already saved the tab, and since saving the tab hasn't finished yet,
  // concludes that we haven't. So this rapidly closed tab don't get removed
  // from Emacs. To resolve this, we need a `savingTabs` which keeps track if
  // tabs which are under-process of being saved.
  savingTabs: number[];
}

/**
 * Events known to Spookfox.
 * For documentation and ease of refactoring.
 */
export enum SFEvents {
  // Emacs sends a CONNECTED request when it connects. Browser don't tell
  // us when it is able to connect to the native app. I suppose it is implicit
  // that if it don't disconnects, it connects. Sounds like ancient wisdom.
  EMACS_CONNECTED = 'EMACS_CONNECTED',
  DISCONNECTED = 'DISCONNECTED',
  // A request Emacs sent to do something or to provide some information
  REQUEST = 'REQUEST',
  // Response Emacs sent for a request we made
  RESPONSE = 'RESPONSE',
  // Spookfox has had a state change, and new state is available
  NEW_STATE = 'NEW_STATE',
}

/**
 * A custom event which has an optional payload attached.
 */
export class SFEvent<P = any> extends Event {
  constructor(
    public name: string,
    public payload?: P,
    public debugMessage?: string
  ) {
    super(name);
  }
}

/**
 * `Spookfox` is the heart of this addon.
 * # Usage
 * 1. Keep a single Spookfox instance. Spookfox will create its own `browser.runtime.port.Port`
 * ```
 * const sf = new Spookfox();
 * ```
 * 2. Create a `browser.runtime.port.Port` if you want multiple Spookfox instances around.
 * e.g for multiple windows?
 * ```
 * const port = browser.runtime.connectNative('spookfox');
 * const sf = new Spookfox(port);
 * ```
 * # Events
 * It emits `SFEvents`. `SFEvents.REQUEST` and `SFEvents.RESPONSE` don't
 * need to be handled manually. `Spookfox.request` and `Spookfox.registerReqHandler`
 * should be sufficient for most cases.
 */
// It extends `EventTarget` so we can have the ability to emit and listen to
// custom events. We rely on custom events to build independent modules, while
// providing a unified interface.
export class Spookfox extends EventTarget {
  state: State = {
    openTabs: {},
    savedTabs: {},
    savingTabs: [],
  };
  reqHandlers = {};
  debug: boolean;

  constructor(public port?: browser.runtime.Port, public windowId = 1) {
    super();
    if (!port) {
      this.port = this.connect();
    }
    this.debug = Boolean(localStorage.getItem('SPOOKFOX_DEBUG'));
    this.init();
  }

  private init() {
    setupBroker(this);
    this.setupEventListeners();
  }

  private reInit() {
    this.port = this.connect();
    this.teardownEventListeners();
    this.init();
  }

  private setupEventListeners() {
    this.addEventListener(SFEvents.REQUEST, this.handleRequest);
    this.addEventListener(SFEvents.RESPONSE, this.handleResponse);
    this.addEventListener(SFEvents.EMACS_CONNECTED, this.handleConnected);
    this.addEventListener(SFEvents.DISCONNECTED, this.handleDisconnected);
  }

  private teardownEventListeners() {
    this.removeEventListener(SFEvents.REQUEST, this.handleRequest);
    this.removeEventListener(SFEvents.RESPONSE, this.handleResponse);
    this.removeEventListener(SFEvents.EMACS_CONNECTED, this.handleConnected);
    this.removeEventListener(SFEvents.DISCONNECTED, this.handleDisconnected);
  }

  private connect() {
    if (this.port) this.port.disconnect();

    return browser.runtime.connectNative('spookfox');
  }

  /**
   * Send a request with NAME and PAYLOAD to Emacs.
   * Returns a promise of response returned by Emacs.
   * # Example
   * ```
   * const savedTabs = sf.request('GET_SAVED_TABS');
   * ```
   */
  async request(name: string, payload?: object) {
    const request = {
      id: uuid(),
      name,
      payload,
    };

    this.port.postMessage(request);

    try {
      const res = await this.getResponse(request.id);
      return res;
    } catch (err) {
      // If the response times-out, it is quite likely #14 happening. Try to
      // brush it-off by reinitializing spookfox
      if (/timeout/.test(err.message.toLowerCase())) {
        console.warn('Response timed out. Reinitializing Spookfox');
        this.reInit();
        this.port.postMessage(request);
        return this.getResponse(request.id);
      }

      throw err;
    }
  }

  /**
   * A convenience function for emitting new events to `Spookfox`.
   */
  emit(name: SFEvents, payload?: object, msg?: string) {
    this.dispatchEvent(new SFEvent(name, payload, msg));
  }

  /**
   * Run a function when Emacs makes a request.
   * # Example
   * ```
   * sf.registerReqHandler('OPEN_TAB', ({ url }) => {
   *   // somehow open a new tab with `url` provided by Emacs.
   * })
   * ```
   */
  registerReqHandler(
    name: string,
    handler: (payload: any, sf: Spookfox) => object
  ) {
    if (this.reqHandlers[name]) {
      throw new Error(
        `Handler already registered. There can only by one handler per request. [request=${name}]`
      );
    }

    this.reqHandlers[name] = handler;
  }

  /**
   * Change Spookfox state. Calling this will set the state to new given state,
   * and emit `SFEvents.NEW_STATE` event.
   * Spookfox.state should be treated as immutable and shouldn't be modified in-place.
   * Instead, use `Spookfox.newState(s: State)` to replace existing state as a whole.
   * # Example
   * ```
   * const newState = { ... };
   * sf.newState(newState, 'X kind of change.');
   * ```
   */
  newState(s: State, debugMsg?: string) {
    this.state = s;
    this.emit(SFEvents.NEW_STATE, s, debugMsg);
  }

  /**
   * Handle `SFEvents.REQUEST` events.
   */
  private handleRequest = async (e: SFEvent<Request>) => {
    const request = e.payload;
    const executioner = this.reqHandlers[request.name];

    if (!executioner) {
      console.warn(
        `No handler for request [request=${JSON.stringify(request)}]`
      );
      return;
    }
    const response = await executioner(request.payload, this);

    return this.port.postMessage({
      requestId: request.id,
      payload: response,
    });
  };

  /**
   * Handle `SFEvents.RESPONSE` events.
   */
  private handleResponse(e: SFEvent<Response>) {
    const res = e.payload;

    if (!res.requestId) {
      throw new Error(`Invalid response: [res=${res}]`);
    }

    // Emit a unique event per `requestId`. Shenanigans I opted for doing
    // to build a promise based interface on request/response dance needed
    // for communication with Emacs. Check `Spookfox.getResponse`
    this.emit(res.requestId as SFEvents, res.payload);
  }

  private getResponse(requestId: string) {
    const maxWait = 2000;

    return new Promise((resolve, reject) => {
      const listener = (event: SFEvent) => {
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
  }

  /**
   * Initialize `Spookfox.state` When Emacs first connects.
   */
  private async handleConnected() {
    const savedTabs = (await this.request('GET_SAVED_TABS')) as SFTab[];
    const currentTabs = await browser.tabs.query({ windowId: this.windowId });

    // Problem: There might be tabs with same URLs
    // Solution: First open tab in browser is mapped to first tab saved in Emacs.
    // Catch: Every time this function runs, all current tabs which match urls
    // saved in Emacs are mapped; regardless of whether user meant it or not.
    const takenSavedTabIds = [];
    const openTabs = currentTabs.reduce((accum, tab) => {
      const savedTab = savedTabs.find(
        (st) => st.url === tab.url && takenSavedTabIds.indexOf(st.id) === -1
      );
      if (savedTab) {
        takenSavedTabIds.push(savedTab.id);
        accum[tab.id] = {
          savedTabId: savedTab.id,
          ...tab,
        };
      } else {
        accum[tab.id] = tab;
      }

      return accum;
    }, {});

    const savedTabsMap = savedTabs.reduce((accum, tab) => {
      accum[tab.id] = tab;
      return accum;
    }, {});

    const initialState = {
      ...this.state,
      openTabs,
      savedTabs: savedTabsMap,
    };

    this.dispatch(Actions.INITIAL_STATE, initialState);
  }

  /**
   * Handle disconnection from spookfox.
   */
  private handleDisconnected(err?: SFEvent) {
    console.warn('Spookfox disconnected. [err=', err, ']');
  }

  private rootReducer({ name, payload }: Action, state: State): State {
    if (this.debug) {
      console.groupCollapsed(name);
      console.log('Payload', payload);
    }

    let nextState = state;
    switch (name) {
      case Actions.INITIAL_STATE:
        nextState = payload;
        break;

      case Actions.SAVE_TAB_START:
        nextState = {
          ...state,
          savingTabs: [...state.savingTabs, payload],
        };
        break;

      case Actions.SAVE_TAB_SUCCESS:
        nextState = {
          ...state,
          savedTabs: {
            ...state.savedTabs,
            [payload.savedTab.id]: payload.savedTab,
          },
          openTabs: {
            ...state.openTabs,
            [payload.openedTab.id.toString()]: {
              ...payload.openedTab,
              savedTabId: payload.savedTab.id,
            },
          },
          savingTabs: state.savingTabs.filter(
            (id) => id !== payload.openedTab.id
          ),
        };
        break;

      case Actions.UPDATE_TAB_START:
        nextState = {
          ...state,
          openTabs: {
            ...state.openTabs,
            [payload.tabId]: {
              ...state.openTabs[payload.tabId],
              ...payload.patch,
            },
          },
        };
        break;

      case Actions.UPDATE_TAB_SUCCESS:
        nextState = {
          ...state,
          savedTabs: {
            ...state.savedTabs,
            [payload.id]: payload,
          },
        };
        break;

      case Actions.REMOVE_TAB_SUCCESS: {
        const savedTabs = { ...state.savedTabs };
        const openTabs = { ...state.openTabs };
        delete openTabs[
          Object.values(openTabs).find((tab) => tab.savedTabId === payload.id)
            ?.id
        ];
        delete savedTabs[payload.id];
        nextState = {
          ...state,
          savedTabs,
          openTabs,
        };
        break;
      }
    }

    console.log('Next state', nextState);
    console.groupEnd();

    return nextState;
  }

  dispatch(name: Actions, payload: any) {
    const newState = this.rootReducer({ name, payload }, this.state);
    this.newState(newState);
  }
}

export enum Actions {
  INITIAL_STATE = 'INITIAL_STATE',
  SAVE_TAB_START = 'SAVE_TAB_INIT',
  SAVE_TAB_SUCCESS = 'SAVE_TAB_SUCCESS',
  SAVE_TAB_FAIL = 'SAVE_TAB_FAIL',
  UPDATE_TAB_START = 'UPDATE_TAB_START',
  UPDATE_TAB_SUCCESS = 'UPDATE_TAB_SUCCESS',
  UPDATE_TAB_FAIL = 'UPDATE_TAB_FAIL',
  REMOVE_TAB_START = 'REMOVE_TAB_START',
  REMOVE_TAB_SUCCESS = 'REMOVE_TAB_SUCCESS',
  REMOVE_TAB_FAIL = 'REMOVE_TAB_FAIL',
}

export enum EmacsRequests {
  TOGGLE_TAB_CHAINING = 'TOGGLE_TAB_CHAINING',
}

interface Action {
  name: Actions;
  payload?: any;
}
