use chrome_native_messaging as cn;
use cn::send;
use serde_json::json;
use std::io::{self, prelude::*, stdin};
use std::os::unix::net::UnixStream;
use std::panic;

use spookfox_native::{packet::*, spawn_socket_server};

/// Handles a panic in the application code, by sending
/// a message back to Chrome before exiting.
fn handle_panic(info: &std::panic::PanicInfo) {
    let msg = match info.payload().downcast_ref::<&'static str>() {
        Some(s) => *s,
        None => match info.payload().downcast_ref::<String>() {
            Some(s) => &s[..],
            None => "Box<Any>",
        },
    };
    send!({
        "status": "panic",
        "payload": msg,
        "file": info.location().map(|l| l.file()),
        "line": info.location().map(|l| l.line())
    });
}

fn run() -> io::Result<()> {
    let socket_path = "/tmp/spookfox.socket";

    spawn_socket_server(socket_path)?;
    panic::set_hook(Box::new(handle_panic));

    // Here we block the tty because browser native-messaging need us to accept
    // input at stdin and provide output at stdout. We don't do any other work
    // here; we take what browser gave us on stdin and send it to the socket
    // server as `Sender::Browser`. Socket server then decides what to do with
    // it, which is usually broadcasting to all connected clients (including
    // Emacs).
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
