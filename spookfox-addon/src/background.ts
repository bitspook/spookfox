import OrgTabs from './apps/OrgTabs';
import { SFEvents, Spookfox } from './Spookfox';
import iconEmacsMono from './icons/emacs-mono.svg';
import iconEmacsColor from './icons/emacs-color.svg';

const run = async () => {
  const sf = ((window as any).spookfox = new Spookfox());

  sf.registerReqHandler('ENABLE_APP', (name: string) => {
    switch (name) {
      case 'org-tabs': {
        sf.registerApp('org-tabs', OrgTabs);
        return { status: 'ok' };
      }
      default:
        return { status: 'error', message: `Uknown app ${name}` };
    }
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
