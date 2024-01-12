import { Draft } from 'immer';
import browser from 'webextension-polyfill';
import { SFApp, Spookfox } from '~src/Spookfox';

type State = undefined;

interface SFWindow {
  id: number;
  title: string;
  isIcognito: boolean;
}

/**
 * App to work with multiple browser windows.
 */
export default class Windows implements SFApp<State> {
  initialState = undefined;

  constructor(public name: string, public sf: Spookfox) {
    sf.registerReqHandler(EmacsRequests.GET_ALL_WINDOWS, this.getAllWindows);
  }

  dispatch(name: Actions, payload: unknown) {
    return this.sf.dispatch(`${this.name}/${name}`, payload);
  }

  async currentWindowId() {
    const currentWindow = await browser.windows.getCurrent();

    return currentWindow.id;
  }

  getAllWindows = async (): Promise<SFWindow[]> => {
    const windows = await browser.windows.getAll();

    return windows.map((w) => ({
      id: w.id,
      title: w.title,
      isIcognito: w.incognito,
    }));
  };

  /**
   * Initialize the state.
   */
  init = async () => {
    this.dispatch(Actions.INIT, null);
  };

  reducer(_, state: Draft<State>) {
    return state;
  }
}

export enum Actions {
  INIT = 'INIT',
}

export enum EmacsRequests {
  GET_ALL_WINDOWS = 'WINDOWS_GET_ALL',
}
