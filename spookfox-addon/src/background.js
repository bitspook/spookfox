const port = browser.runtime.connectNative('spookfox');
port.onDisconnect.addListener((p) => {
    console.log('Disconnected from Native APP');
    if (p.error) {
        console.error('Disconnected due to error', p.error);
    }
});
port.onMessage.addListener(async (msg) => {
    if (!msg.payload) {
        console.warn('Unknown message:', msg);
        return;
    }
    try {
        const action = JSON.parse(msg.payload);
        switch (action.type) {
            case 'GET_ACTIVE_TAB': {
                const msg_1 = await getActiveTab();
                return port.postMessage(msg_1);
            }
            case 'GET_ALL_TABS': {
                const msg_2 = await getAllTabs();
                return port.postMessage(msg_2);
            }
            default:
                console.warn(`Unknown action [action=${JSON.stringify(action)}]`);
        }
    }
    catch (err) {
        console.error(`Bad message payload [err=${err}, msg=${msg.payload}]`);
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
    };
};
const getAllTabs = async () => {
    const tabs = await browser.tabs.query({ currentWindow: true });
    return tabs.map((tab) => ({
        url: tab.url,
        title: tab.title,
    }));
};
browser.browserAction.onClicked.addListener(async () => {
    console.warn('TABS', await getActiveTab());
});
