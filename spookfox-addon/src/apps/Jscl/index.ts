import { Draft, Immutable } from 'immer';
import { SFApp, Spookfox } from '~src/Spookfox';

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
    const timerLabel = `Evaluating`;
    try {
      console.time(timerLabel);
      console.log(lispStr);
      return (window as any).jscl.evaluateString(lispStr);
    } catch (err) {
      console.error('Error occurred while evaluating JSCL', err);
      return {
        error: 'Failed to eval CL. Check addon console.',
        message: err.message,
      };
    } finally {
      console.timeEnd(timerLabel);
    }
  };

  reducer(_action: any, _state: Draft<JsclState>) {
    return this.initialState;
  }
}

export enum Actions {}

export enum EmacsRequests {
  EVAL_BG = 'JSCL_EVAL_BG',
}
