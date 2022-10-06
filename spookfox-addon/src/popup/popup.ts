const button = document.querySelector('.reconnect');
const statusDot = document.querySelector('.status > #dot');
const statusMsg = document.querySelector('.status > #msg');

const reconnect = async () => {
  const sf = (await browser.runtime.getBackgroundPage()).spookfox;
  sf.reConnect();

  setTimeout(() => button.classList.remove('rotating'), 2000);
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
  const sf = (await browser.runtime.getBackgroundPage()).spookfox;

  if (!sf) {
    console.warn('NO Spookfox');
    return;
  }

  if (sf.isConnected) {
    handleConnected();
  } else {
    handleDisconnected();
  }

  sf.addEventListener('CONNECTED', handleConnected);
  sf.addEventListener('CONNECTING', handleConnecting);
  sf.addEventListener('DISCONNECTED', handleDisconnected);
};

init();

button.addEventListener('click', reconnect);
