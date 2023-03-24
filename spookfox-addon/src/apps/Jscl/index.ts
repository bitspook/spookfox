import { Draft, Immutable } from 'immer';
import { SFApp, Spookfox } from '~src/Spookfox';
// JSCL.evalToString does not work if jscl is included directly in
// manifest->background->scripts. In that case, JSCL tries to use JS's eval and
// browser throws a CSP error
try {
  // Loading JSCL is throwing an error, although it seems to be capable of doing
  // its job
  require('./jscl');
} catch (err) {
  /* pass */
}

export type JsclState = Immutable<null>;

export default class Jscl implements SFApp<JsclState> {
  initialState: Immutable<JsclState> = null;

  get state(): JsclState {
    return this.sf.state[this.name];
  }

  dispatch(name: Actions, payload: unknown) {
    return this.sf.dispatch(`${this.name}/${name}`, payload);
  }

  constructor(public name: string, public sf: Spookfox) {
    sf.registerReqHandler(EmacsRequests.EVAL_BG, this.evalInBackgroundScript);
  }

  evalInBackgroundScript = async (lispStr: string) => {
    return (window as any).jscl.evaluateString(lispStr);
  };

  reducer(_action: any, _state: Draft<JsclState>) {
    return this.initialState;
  }
}

export enum Actions {}

export enum EmacsRequests {
  EVAL_BG = 'JSCL_EVAL_BG',
}
