use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Serialize, Deserialize)]
pub enum Sender {
    Emacs,
    Browser,
}

#[derive(Serialize, Deserialize)]
pub enum Status {
    Error,
    Success,
}

#[derive(Serialize, Deserialize)]
pub enum MsgType {
    Request,
    Response,
}

#[derive(Serialize, Deserialize)]
pub struct Packet {
    pub status: Status,
    pub sender: Sender,
    pub message: Value,
}
