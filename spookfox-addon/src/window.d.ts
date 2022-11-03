import { Spookfox } from './Spookfox';

declare global {
  interface Window {
    spookfox: Spookfox;
  }
}

export {};
