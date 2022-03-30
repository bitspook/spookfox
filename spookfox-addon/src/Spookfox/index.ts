import produce, { Immutable } from 'immer';
import { v4 as uuid } from 'uuid';
import { gobbleErrorsOf } from '~src/lib';
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

export interface SFApp<S> {
  name: string;
  initialState: Immutable<S>;
  reducer: (action: { name: string; payload: any }, state: S) => S;
}

export interface SFAppConstructor<S> {
  new (name: string, sf: Spookfox): SFApp<S>;
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
  constructor(public name: string, public payload?: P) {
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
  state = {};
  reqHandlers = {};
  // This is needed only for re-init hack
  eventListeners = [];
  debug: boolean;
  apps: { [name: string]: SFApp<any> } = {};

  constructor(public port?: browser.runtime.Port) {
    super();
    if (!port) {
      this.port = this.reConnect();
    }
    this.debug = Boolean(localStorage.getItem('SPOOKFOX_DEBUG'));
    setupBroker(this);
    this.setupEventListeners();
  }

  private reInit() {
    this.port = this.reConnect();

    for (const l of this.eventListeners) {
      this.removeEventListener(l.type, l.callback);
      super.addEventListener(l.type, l.callback);
    }

    setupBroker(this);
  }

  addEventListener(
    type: string,
    callback: EventListenerOrEventListenerObject
  ): void {
    this.eventListeners.push({ type, callback });
    super.addEventListener(type, callback);
  }

  removeEventListener(
    type: string,
    callback: EventListenerOrEventListenerObject
  ): void {
    this.eventListeners = this.eventListeners.filter(
      (el) => !(el.type === type && el.callback === callback)
    );
    super.removeEventListener(type, callback);
  }

  private setupEventListeners() {
    this.addEventListener(SFEvents.REQUEST, gobbleErrorsOf(this.handleRequest));
    this.addEventListener(
      SFEvents.RESPONSE,
      gobbleErrorsOf(this.handleResponse)
    );
    this.addEventListener(
      SFEvents.DISCONNECTED,
      gobbleErrorsOf(this.handleDisconnected)
    );
  }

  private reConnect() {
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

    return this.getResponse(request.id);
  }

  /**
   * A convenience function for emitting new events to `Spookfox`.
   */
  emit(name: SFEvents, payload?: object) {
    this.dispatchEvent(new SFEvent(name, payload));
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

  registerApp<S>(name: string, App: SFAppConstructor<S>) {
    this.apps[name] = new App(name, this);
    this.state[name] = this.apps[name].initialState;
  }

  /**
   * Change Spookfox state. Calling this will set the state to new given state,
   * and emit `SFEvents.NEW_STATE` event.
   * Spookfox.state should be treated as immutable and shouldn't be modified in-place.
   * # Example
   * ```
   * const newState = { ... };
   * sf.newState(newState, 'X kind of change.');
   * ```
   */
  private newState(s: any) {
    this.state = s;
    this.emit(SFEvents.NEW_STATE, s);
  }

  /**
   * Handle `SFEvents.REQUEST` events.
   */
  private handleRequest = async (e: SFEvent<Request>) => {
    const request = e.payload;
    const executioner = this.reqHandlers[request.name];

    if (!executioner) {
      console.warn('No handler for request', { request });
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
  private handleResponse = async (e: SFEvent<Response>) => {
    const res = e.payload;

    if (!res.requestId) {
      throw new Error(`Invalid response: [res=${res}]`);
    }

    // Emit a unique event per `requestId`. Shenanigans I opted for doing
    // to build a promise based interface on request/response dance needed
    // for communication with Emacs. Check `Spookfox.getResponse`
    this.emit(res.requestId as SFEvents, res.payload);
  };

  private getResponse = (requestId: string) => {
    const maxWait = 5000;

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
  };

  /**
   * Handle disconnection from spookfox.
   */
  private handleDisconnected = async (event?: SFEvent) => {
    console.warn('Spookfox disconnected.', { event });
  };

  private rootReducer({ name, payload }: Action, state: any): any {
    const [appName, actionName] = name.split('/');

    if (!appName || !actionName) {
      throw new Error(
        'Invalid Action "`${name}`". Action should be in format "<app-name>/<action-name>"'
      );
    }

    if (this.debug) {
      console.groupCollapsed(name);
      console.log('Payload', payload);
    }

    const app = this.apps[appName];

    if (!app) {
      throw new Error(
        `Could not find Spookfox app "${appName}". Was it registered?`
      );
    }

    const nextState = produce(this.state, (draft: typeof app.initialState) => {
      draft[appName] = app.reducer(
        { name: actionName, payload },
        draft[appName]
      );
    });

    if (this.debug) {
      console.log('Next state', nextState);
      console.groupEnd();
    }

    return nextState;
  }

  dispatch(name: string, payload: any) {
    // Need to manually do the error handling here because Firefox is eating
    // these errors up and not showing them in addon's console
    try {
      const newState = this.rootReducer({ name, payload }, this.state);
      this.newState(newState);
    } catch (err) {
      console.error('Error during dispatching action, [err=', err, ']');
    }
  }
}

export enum EmacsRequests {
  TOGGLE_TAB_CHAINING = 'TOGGLE_TAB_CHAINING',
}

interface Action {
  name: string;
  payload?: any;
}
