import { SFEvents, Spookfox } from './Spookfox';
// eslint-disable-next-line
import iconEmacsMono from './icons/emacs-mono.svg';
import iconEmacsColor from './icons/emacs-color.svg';
import Tabs from './apps/Tabs';
import OrgTabs from './apps/OrgTabs';
import JsInject from './apps/JsInject';

const run = async () => {
  const sf = ((window as any).spookfox = new Spookfox());

  sf.registerReqHandler('ENABLE_APP', (name: string) => {
    switch (name) {
      case 'spookfox-tabs': {
        sf.registerApp('tabs', Tabs);
        break;
      }
      case 'spookfox-org-tabs': {
        sf.registerApp('org-tabs', OrgTabs);
        break;
      }
      case 'spookfox-js-injection': {
        sf.registerApp('js-injection', JsInject);
        break;
      }
      default:
        return { status: 'error', message: `Uknown app ${name}` };
    }

    return { status: 'ok' };
  });

  let autoConnectInterval = null;
  sf.addEventListener(SFEvents.CONNECTED, () => {
    browser.browserAction.setIcon({ path: iconEmacsColor });

    if (autoConnectInterval) clearInterval(autoConnectInterval);
  });

  sf.addEventListener(SFEvents.DISCONNECTED, () => {
    browser.browserAction.setIcon({ path: iconEmacsMono });

    autoConnectInterval = setInterval(() => {
      sf.reConnect();
    }, 5000);
  });
};

run().catch((err) => {
  console.error('An error occurred in run()', err);
});
