import { produce, Immutable } from 'immer';
import { v4 as uuid } from 'uuid';

interface ErrorResPayload {
  status: 'error';
  message: string;
}

interface Response {
  type: 'response';
  requestId: string;
  payload: ErrorResPayload | any;
}

interface Request {
  id: string;
  name: string;
  payload: any;
  type: 'request';
}

export interface SFApp<S> {
  name: string;
  initialState: Immutable<S>;
  reducer: (action: { name: string; payload: any }, state: S) => S;
  init?: () => void;
}

export interface SFAppConstructor<S> {
  new (name: string, sf: Spookfox): SFApp<S>;
}

/**
 * Events known to Spookfox.
 * For documentation and ease of refactoring.
 */
export enum SFEvents {
  CONNECTED = 'CONNECTED',
  CONNECTING = 'CONNECTING',
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
    public payload?: P
  ) {
    super(name);
  }
}

enum LogLevel {
  Error = 0,
  Info = 1,
  Debug = 2,
}

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
export class Spookfox extends EventTarget {
  state = {};
  reqHandlers = {};
  // This is needed only for re-init hack
  eventListeners = [];
  debugLevel: LogLevel;
  apps: { [name: string]: SFApp<any> } = {};
  wsUrl = 'ws://localhost:59001';
  ws: WebSocket;
  reConnecting = false;

  constructor() {
    super();
    this.ws = this.reConnect();
    this.setupEventListeners();
  }

  get isConnected() {
    return this.ws && this.ws.readyState === this.ws.OPEN;
  }

  get logLevel(): LogLevel {
    return (
      parseInt(localStorage.getItem('SPOOKFOX_DEBUG'), 10) || LogLevel.Error
    );
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
    this.addEventListener(SFEvents.REQUEST, this.handleRequest);
    this.addEventListener(SFEvents.RESPONSE, this.handleResponse);
  }

  handleServerMsg = async (event: MessageEvent<string>) => {
    try {
      const msg = JSON.parse(event.data);

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
  async request(name: string, payload?: object) {
    if (!this.ws) {
      console.error(
        `Not connected to Spookfox Server. Dropping request ${name}.`
      );
      return;
    }

    const request = {
      id: uuid(),
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
  emit = (name: SFEvents, payload?: object) => {
    const event = new SFEvent(name, payload);

    if (this.logLevel >= LogLevel.Debug) {
      console.log('Emitting', event);
    }

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
  registerReqHandler(
    name: string,
    handler: (payload: any, sf: Spookfox) => object
  ) {
    if (this.reqHandlers[name] && this.logLevel) {
      console.warn(`Overwriting handler for: ${name}`);
    }

    this.reqHandlers[name.toUpperCase()] = handler;
  }

  registerApp<S>(name: string, App: SFAppConstructor<S>) {
    if (this.apps[name]) return;
    this.apps[name] = new App(name, this);

    if (typeof this.apps[name].init === 'function') this.apps[name].init();

    this.state = produce(this.state, (state) => {
      state[name] = this.apps[name].initialState;
    });
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
  private replaceState(s: any) {
    this.state = s;
    this.emit(SFEvents.NEW_STATE, s);
  }

  /**
   * Handle `SFEvents.REQUEST` events.
   */
  private handleRequest = async (e: SFEvent<Request>) => {
    const request = e.payload;

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
  private handleResponse = async (e: SFEvent<Response>) => {
    const res = e.payload;

    if (!res.requestId) {
      throw new Error(`Invalid response: [res=${JSON.stringify(res)}]`);
    }

    // Emit a unique event per `requestId`. Shenanigans I opted for doing
    // to build a promise based interface on request/response dance needed
    // for communication with Emacs. Check `Spookfox.getResponse`
    this.emit(res.requestId as SFEvents, res);
  };

  private getResponse = (requestId: string): Promise<Response> => {
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

  private rootReducer({ name, payload }: Action): any {
    const [appName, actionName] = name.split('/');

    if (!appName || !actionName) {
      throw new Error(
        'Invalid Action "`${name}`". Action should be in format "<app-name>/<action-name>"'
      );
    }

    if (this.logLevel) {
      console.groupCollapsed(name);
      console.log('Payload', payload);
    }

    const app = this.apps[appName];

    if (!app) {
      console.log('APPS', app, appName, this.apps[appName]);
      console.groupEnd();
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

    if (this.logLevel) {
      console.log('Next state', nextState);
      console.groupEnd();
    }

    return nextState;
  }

  dispatch(name: string, payload: any) {
    // Need to manually do the error handling here because Firefox is eating
    // these errors up and not showing them in addon's console
    try {
      const newState = this.rootReducer({ name, payload });
      this.replaceState(newState);
    } catch (err) {
      console.error('Error during dispatching action, [err=', err, ']');
    }
  }
}

interface Action {
  name: string;
  payload?: any;
}
