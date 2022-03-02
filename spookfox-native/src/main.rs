use chrome_native_messaging as cn;
use cn::send;
use serde_json::json;
use std::io::{self, prelude::*, stdin};
use std::os::unix::net::UnixStream;

use spookfox_native::{packet::*, spawn_socket_server};

fn run() -> io::Result<()> {
    let socket_path = "/tmp/spookfox.socket";

    spawn_socket_server(socket_path)?;

    // We use socket to communicate with ourselves. Every message browser sends
    // is sent to the socket server as `Sender: Browser`. Socket server then
    // decides what to do with it, which is usually broadcasting to all
    // connected clients (including Emacs).
    let mut socket = UnixStream::connect(socket_path)?;

    // Let's ignore any JSON formatting errors from browser
    while let Ok(val) = cn::read_input(stdin()) {
        let msg = Packet {
            status: Status::Success,
            sender: Sender::Browser,
            message: val,
        };

        socket.write_all((json!(msg).to_string() + "\n").as_bytes())?;
        socket.flush()?;
    }

    Ok(())
}

fn main() {
    if let Err(err) = run() {
        send!({ "error": format!("{}", err) });
    }
}
