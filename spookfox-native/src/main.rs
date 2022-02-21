use chrome_native_messaging as cn;
use cn::send;
use serde_json::json;
use std::io::{self, prelude::*, stdin};
use std::os::unix::net::UnixStream;
use std::panic;

use spookfox_native::{spawn_socket_server, types::*};

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

    spawn_socket_server(socket_path)?;

    let mut stream = UnixStream::connect(socket_path)?;
    loop {
        match cn::read_input(stdin()) {
            Err(err) => {
                if let cn::Error::NoMoreInput = err {
                    break;
                }
                send!({ "error": format!("{}", err) });

                let msg = format!("{}", err);
                let msg = SFMsg::from_browser(msg, true);
                stream.write_all(msg.as_bytes())?;
                stream.flush()?;
            }
            Ok(val) => {
                let msg = format!("{}", val);
                let msg = SFMsg::from_browser(msg, false);

                stream.write_all(msg.as_bytes())?;
                stream.flush()?;
            }
        }
    }

    Ok(())
}

fn main() {
    if let Err(err) = run() {
        send!({ "error": format!("{}", err) });
    }
}
