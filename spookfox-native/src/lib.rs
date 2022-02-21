use chrome_native_messaging as cn;
use serde_json::json;
use std::sync::mpsc;
use std::{
    io::{self, prelude::*, BufReader},
    os::unix::net::UnixListener,
    os::unix::net::UnixStream,
};

pub mod types;

use types::*;

struct NewSocketConnection {
    connection: UnixStream,
}

enum CrossThreadMsgs {
    SocketCon(NewSocketConnection),
    SocketMsg(SFMsg),
}

pub fn spawn_socket_server(socket_path: &str) -> io::Result<()> {
    if std::fs::remove_file(socket_path).is_ok() {
        eprintln!("Deleted existing socket: {}", socket_path);
    }

    let socket = UnixListener::bind(socket_path)?;
    let (tx, rx) = mpsc::channel();

    std::thread::spawn(move || {
        socket.incoming().into_iter().for_each(|connection| {
            if let Ok(stream) = connection {
                let listener = stream.try_clone().unwrap();

                tx.send(CrossThreadMsgs::SocketCon(NewSocketConnection {
                    connection: stream,
                }))
                .unwrap();

                let tx = tx.clone();
                std::thread::spawn(move || {
                    let listener = BufReader::new(&listener);

                    listener.lines().into_iter().for_each(|line| {
                        if let Ok(line) = line {
                            match serde_json::from_str::<SFMsg>(line.trim()) {
                                Ok(msg) => {
                                    // We don't care about other communication on
                                    // the socket. Only messages sent from Emacs
                                    // should be passed to Browser.
                                    if let SFComponent::Emacs = msg.sender {
                                        cn::send!(msg);
                                    } else {
                                        tx.send(CrossThreadMsgs::SocketMsg(msg)).unwrap();
                                    }
                                }
                                Err(err) => {
                                    let payload = format!(
                                        "Received invalid JSON  [err={}, line={}]",
                                        err, line
                                    );
                                    let msg = SFMsg {
                                        msg_type: SFMsgType::Error,
                                        sender: SFComponent::Native,
                                        payload,
                                    };
                                    cn::send!(msg)
                                }
                            }
                        }
                    });
                });
            }
        });
    });

    std::thread::spawn(|| {
        let mut connections = vec![];
        for msg in rx {
            match msg {
                CrossThreadMsgs::SocketCon(conn) => connections.push(conn.connection),
                CrossThreadMsgs::SocketMsg(msg) => {
                    for mut conn in &connections {
                        conn.write_all((json!(msg).to_string() + "\n").as_bytes())?;
                        conn.flush()?;
                    }
                }
            }
        }

        io::Result::Ok(())
    });

    Ok(())
}
