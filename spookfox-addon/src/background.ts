import OrgTabs from './apps/OrgTabs';
import { Spookfox } from './Spookfox';

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
};

run().catch((err) => {
  console.error('An error occurred in run()', err);
});
