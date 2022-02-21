var port = browser.runtime.connectNative("spookfox");

port.onDisconnect.addListener((p) => {
  console.log('Disconnected from Native APP');
  if (p.error) {
    console.error('Disconnected due to error', p.error);
  }
})

port.onMessage.addListener((msg) => {
  if (!msg.payload) {
    console.warn('Unknown message:', msg);
    return;
  }

  try {
    const action = JSON.parse(msg.payload);

    switch (action.type) {
      case 'GET_ACTIVE_TAB':
        return getActiveTab().then(msg => port.postMessage(msg));
      default:
        console.warn(`Unknown action [action=${JSON.stringify(action)}]`)
    }

  } catch (err) {
    console.err(`Bad message payload [err=${err}, msg=${msg.payload}]`);
  }
});

const getActiveTab = async () => {
  const tabs = await browser.tabs.query({ currentWindow: true, active: true });
  if (!tabs.length) {
    return { type: 'Error', message: 'No active tabs found' };
  }

  const activeTab = tabs[0];

  return {
    url: activeTab.url,
    title: activeTab.title,
  }
};

browser.browserAction.onClicked.addListener(async () => {
  console.warn('TABS', await getActiveTab());
});
