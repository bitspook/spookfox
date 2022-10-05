import { gobbleErrorsOf } from '~src/lib';
import { Spookfox, SFEvents } from './index';

interface Packet {
  status: 'Success' | 'Error';
  message: string;
  sender: string;
}

/**
 * Broker is responsible for handling all communication with spookfox-native.
 * It receives packets from spookfox-native, parses the packets to extract enough
 * meaning from them so that it can dispatch appropriate event to `Spookfox`.
 * Actual work in response to those events should be done elsewhere.
 */
export default (sf: Spookfox) => {
  /**
   * Inform `Spookfox` when browser disconnects from spookfox-native for any reason.
   */
  // sf.port.onDisconnect.addListener((p) => {
  //   sf.emit(SFEvents.DISCONNECTED, { error: p.error });
  // });
  sf.ws.onclose((ws: WebSocket, event: CloseEvent) => {
    sf.emit(SFEvents.DISCONNECTED, { event });
  })

  const handleNewMessage = async (event: MessageEvent<string>) => {
    const pkt: Packet = JSON.parse(event.data);
    // I know, thou shall not do any work here; FIXME maybe?
    if (pkt.status === 'Error') {
      console.error('spookfox-server faced an error, [err=', pkt.message, ']');
      return;
    }
    // FIXME too while you're at it.
    if (!pkt.message) {
      console.warn('Unknown message:', pkt);
      return;
    }

    try {
      const msg = JSON.parse(pkt.message);
      // Handle this event as a special case to provide a uniform interface through
      // `Spookfox`.
      if (msg.name === 'CONNECTED') {
        return sf.emit(SFEvents.EMACS_CONNECTED);
      }

      if (msg.name) {
        return sf.emit(SFEvents.REQUEST, msg);
      }

      return sf.emit(SFEvents.RESPONSE, msg);
    } catch (err) {
      console.error(`Bad message payload [err=${err}, msg=${pkt.message}]`);
    }
  };

  sf.ws.onmessage(gobbleErrorsOf(handleNewMessage));
};
