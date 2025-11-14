const button = document.querySelector('.reconnect');
const statusDot = document.querySelector('.status > #dot');
const statusMsg = document.querySelector('.status > #msg');

const reconnect = async (port) => {
  port.postMessage({ type: 'RECONNECT' });
};

const handleConnected = () => {
  statusDot.classList.value = 'connected';
  statusMsg.innerHTML = 'Connected';
  button.classList.remove('rotating');
};

const handleDisconnected = () => {
  statusDot.classList.value = 'disconnected';
  statusMsg.innerHTML = 'Not connected';
  button.classList.remove('rotating');
};

const handleConnecting = () => {
  button.classList.add('rotating');

  statusDot.classList.value = 'connecting';
  statusMsg.innerHTML = 'Connecting...';
};

const init = async () => {
  const port = browser.runtime.connect();

  if (!port) {
    console.warn('No Spookfox port');
    return;
  }

  port.onMessage.addListener((msg) => {
    switch (msg.type) {
      case 'CONNECTED':
        return handleConnected();
      case 'CONNECTING':
        return handleConnecting();
      case 'DISCONNECTED':
        return handleDisconnected();
    }
  });

  button.addEventListener('click', () => reconnect(port));
};

init();
