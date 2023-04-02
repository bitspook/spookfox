import browser from 'webextension-polyfill';

window.addEventListener('message', (event) => {
  if (
    event.data &&
    event.data.type === 'SPOOKFOX_RELAY_TO_EMACS' &&
    event.data.action &&
    typeof event.data.action.name === 'string'
  ) {
    const { type, action } = event.data;

    browser.runtime.sendMessage({ type, action });
  }
});
