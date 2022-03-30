use chrome_native_messaging as cn;
use cn::send;
use serde_json::json;
use std::io::{self, stdin};
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
    panic::set_hook(Box::new(handle_panic));

    let socket_path = "/tmp/spookfox.socket";
    // Cross-thread communication is needed to keep track of all the clients
    // that connect to our socket. We can get away with having just 2 open
    // connections, but for the sake of debugging and future extension, we are
    // accepting n number of connections, where n is limited by number of
    // threads OS can open, and number of connections socket can support
    let (tx, rx) = std::sync::mpsc::channel();
    let main_tx = tx.clone();
    spawn_socket_server(socket_path, tx, rx)?;

    // Let's ignore any JSON formatting errors from browser
    while let Ok(val) = cn::read_input(stdin()) {
        let msg = Packet {
            status: Status::Success,
            sender: Sender::Browser,
            message: val,
        };
        main_tx.send(spookfox_native::CrossThreadMsg::Packet(msg)).unwrap();
    }

    Ok(())
}

fn main() {
    if let Err(err) = run() {
        send!({ "error": format!("{}", err) });
    }
}
