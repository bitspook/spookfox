use chrome_native_messaging as cn;
use serde_json::{json, Value};
use std::sync::mpsc;
use std::{
    io::{self, prelude::*, BufReader},
    os::unix::net::UnixListener,
    os::unix::net::UnixStream,
};

pub mod packet;

use packet::*;

enum CrossThreadMsg {
    Connection(UnixStream),
    Packet(Packet),
}

pub fn spawn_socket_server(socket_path: &str) -> io::Result<()> {
    if std::fs::remove_file(socket_path).is_ok() {
        eprintln!("Deleted existing socket: {}", socket_path);
    }

    let socket = UnixListener::bind(socket_path)?;
    // Cross-thread communication is needed to keep track of all the clients
    // that connect to our socket. We can get away with having just 2 open
    // connections, but for the sake of debugging and future extension, we are
    // accepting n number of connections, where n is limited by number of
    // threads OS can open, and number of connections socket can support
    let (tx, rx) = mpsc::channel();

    std::thread::spawn(move || {
        socket.incoming().into_iter().for_each(|connection| {
            if let Ok(stream) = connection {
                let listener = stream.try_clone().unwrap();

                tx.send(CrossThreadMsg::Connection(stream)).unwrap();

                let tx = tx.clone();
                std::thread::spawn(move || {
                    let listener = BufReader::new(&listener);

                    // We have to devise a way to tell where one message ends
                    // and next starts. Doing so at a new line seems fine since
                    // our packets are serialized JSON documents
                    listener.lines().into_iter().for_each(|line| {
                        if let Ok(line) = line {
                            match serde_json::from_str::<Packet>(line.trim()) {
                                Ok(msg) => match msg.sender {
                                    Sender::Emacs => cn::send!(msg),
                                    Sender::Browser => tx.send(CrossThreadMsg::Packet(msg)).unwrap(),
                                },
                                Err(err) => {
                                    // If a client posts invalid message, like badly
                                    // crafted packets from Emacs; they are
                                    // reported back to the browser since it is
                                    // the primary UI on socket-server side
                                    // (since it starts the socket server)
                                    let message = Value::String(format!(
                                        "Received invalid JSON  [err={}, line={}]",
                                        err, line
                                    ));
                                    let msg = Packet {
                                        status: Status::Error,
                                        // We just assume that sender is Emacs. It might
                                        // very well be that pesky nc, or its
                                        // hipster cousin ncat
                                        sender: Sender::Emacs,
                                        message,
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
                CrossThreadMsg::Connection(conn) => connections.push(conn),
                CrossThreadMsg::Packet(msg) => {
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
