import { Draft, Immutable } from 'immer';
import browser from 'webextension-polyfill';
import { SFApp, Spookfox } from '~src/Spookfox';

export type JsInjectState = Immutable<null>;

export default class JsInject implements SFApp<JsInjectState> {
  initialState: Immutable<JsInjectState> = null;

  get state(): JsInjectState {
    return this.sf.state[this.name];
  }

  dispatch(name: Actions, payload: unknown) {
    return this.sf.dispatch(`${this.name}/${name}`, payload);
  }

  constructor(public name: string, public sf: Spookfox) {
    sf.registerReqHandler(
      EmacsRequests.EVAL_IN_ACTIVE_TAB,
      this.evalJsInActiveTab
    );
    sf.registerReqHandler(
      EmacsRequests.EVAL_IN_BACKGROUND_SCRIPT,
      this.evalJsInBackgroundScript
    );
    sf.registerReqHandler(
      EmacsRequests.EVAL_IN_TAB,
      this.evalJsInTab
    )
  }

  /**
   * Inject Javascript sent by Emacs into active tab and send whatever it
   * returns as response.
   */
  evalJsInActiveTab = async (script: browser.ExtensionTypes.InjectDetails) => {
    const currentWindow = await browser.windows.getCurrent()
    const activeTabs = await browser.tabs.query({ active: true, windowId: currentWindow.id });

    if (!activeTabs.length) {
      throw new Error(
        'No active tab to execute script in. [script=${JSON.stringify(script)}]'
      );
    }

    return Promise.all(
      activeTabs.map((tab) => browser.tabs.executeScript(tab.id, script))
    );
  };

  evalJsInTab = async ({ code, 'tab-id': tabId }: { code: string, 'tab-Id': string }) => {
    return browser.tabs.executeScript(tabId, {code})
  }

  evalJsInBackgroundScript = async ({ code }: { code: string }) => {
    const result = window.eval(code);

    return result;
  };

  reducer(_action: any, _state: Draft<JsInjectState>) {
    return this.initialState;
  }
}

export enum Actions {}

export enum EmacsRequests {
  EVAL_IN_ACTIVE_TAB = 'JS_INJECT_EVAL_IN_ACTIVE_TAB',
  EVAL_IN_BACKGROUND_SCRIPT = 'JS_INJECT_EVAL_IN_BACKGROUND_SCRIPT',
  EVAL_IN_TAB = 'JS_INJECT_EVAL_IN_TAB'
}
