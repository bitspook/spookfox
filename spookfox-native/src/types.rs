use serde::{Serialize, Deserialize};
use serde_json::json;

#[derive(Serialize, Deserialize)]
pub enum SFComponent {
    Emacs,
    Browser,
    Native,
}

#[derive(Serialize, Deserialize)]
pub enum SFMsgType {
    Error,
    Success,
}

#[derive(Serialize, Deserialize)]
pub struct SFMsg {
    #[serde(rename = "type")]
    pub msg_type: SFMsgType,
    pub sender: SFComponent,
    pub payload: String,
}

impl SFMsg {
    pub fn from_browser(msg: String, is_error: bool) -> String {
        let msg = json!(Self {
            msg_type: if is_error {
                SFMsgType::Error
            } else {
                SFMsgType::Success
            },
            sender: SFComponent::Browser,
            payload: msg,
        })
        .to_string();

        format!("{}\n", msg)
    }
}
