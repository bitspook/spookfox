import { SFEvents, Spookfox } from './Spookfox';
// eslint-disable-next-line
import iconEmacsMono from './icons/emacs-mono.svg';
import iconEmacsColor from './icons/emacs-color.svg';
import Tabs from './apps/Tabs';
import OrgTabs from './apps/OrgTabs';
import JsInject from './apps/JsInject';
import browser from 'webextension-polyfill';
import Windows from './apps/Windows';

let autoConnectInterval = null;
let connectedPorts: browser.Runtime.Port[] = [];

// Messages from content script
browser.runtime.onMessage.addListener(
  (msg: { type: string; action: { name: string; payload?: any } }) => {
    const sf = window.spookfox;
    switch (msg.type) {
      case 'SPOOKFOX_RELAY_TO_EMACS': {
        sf.request(msg.action.name, msg.action.payload);
      }
    }
  }
);

// Messages from popup
browser.runtime.onConnect.addListener((port) => {
  connectedPorts.push(port);
  port.postMessage({
    type: window.spookfox.isConnected ? 'CONNECTED' : 'DISCONNECTED',
  });

  port.onDisconnect.addListener(
    () => (connectedPorts = connectedPorts.filter((p) => p !== port))
  );

  port.onMessage.addListener((msg: { type: string }) => {
    const sf = window.spookfox;

    switch (msg.type) {
      case 'RECONNECT':
        return sf.reConnect();
    }
  });
});

const startAutoconnectTimer = (sf: Spookfox) => {
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

const run = async () => {
  const sf = ((window as any).spookfox = new Spookfox());

  // register all available apps
  sf.registerApp('js-injection', JsInject);
  sf.registerApp('tabs', Tabs);
  sf.registerApp('spookfox-windows', Windows);

  startAutoconnectTimer(sf);
};

run().catch((err) => {
  console.error('An error occurred in run()', err);
});
