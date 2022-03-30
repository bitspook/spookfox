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

pub enum CrossThreadMsg {
    Connection(UnixStream),
    Packet(Packet),
}

pub fn spawn_socket_server(
    socket_path: &str,
    tx: mpsc::Sender<CrossThreadMsg>,
    rx: mpsc::Receiver<CrossThreadMsg>,
) -> io::Result<()> {
    if std::fs::remove_file(socket_path).is_ok() {
        eprintln!("Deleted existing socket: {}", socket_path);
    }

    let socket = UnixListener::bind(socket_path)?;

    std::thread::spawn(move || {
        socket.incoming().into_iter().for_each(|connection| {
            if let Ok(stream) = connection {
                let listener = stream.try_clone().unwrap();

                tx.send(CrossThreadMsg::Connection(stream)).unwrap();

                std::thread::spawn(move || {
                    let listener = BufReader::new(&listener);

                    // We have to devise a way to tell where one message ends
                    // and next starts. Doing so at a new line seems fine since
                    // our packets are serialized JSON documents
                    listener.lines().into_iter().for_each(|line| {
                        if let Ok(line) = line {
                            match serde_json::from_str::<Packet>(line.trim()) {
                                Ok(msg) => cn::send!(msg),
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

    std::thread::spawn(move || {
        let mut connections = vec![];
        for msg in rx {
            match msg {
                CrossThreadMsg::Connection(conn) => {
                    connections.push(conn);
                }
                CrossThreadMsg::Packet(msg) => {
                    for mut conn in &connections {
                        conn.flush().unwrap();
                        if conn
                            .write((json!(msg).to_string() + "\n").as_bytes())
                            .is_err()
                        {
                            let msg = Packet {
                                status: Status::Error,
                                sender: Sender::Emacs,
                                message: Value::String(
                                    "Failed to send message to spookfox-native client.".to_string(),
                                ),
                            };
                            cn::send!(msg)
                        }
                    }
                }
            }
        }
    });

    Ok(())
}
