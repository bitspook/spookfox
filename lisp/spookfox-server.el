;;; package --- Communicate with a browser which have spookfox browser addon installed. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;;
;;; Code:
(require 'websocket)
(require 'cl-lib)

(defvar sfs--server-port 59001)
(defvar sfs--clients nil)

(defun sfs--handle-client-open (ws)
  "When a new client connects, save the connected websocket WS."
  (message "New client")
  (push ws sfs--clients))

(defun sfs--handle-client-close (ws)
  "When a client connection closes, remove the websocket WS from saved sockets."
  (setf sfs--clients (cl-remove-if (lambda (saved-ws) (eq saved-ws ws)) sfs--clients))
  (message "Client closed."))

(defun sfs--handle-server-error (ws sym err)
  "Handle WS server error ERR in SYM callback."
  (message "Error %s occurred in %s" err sym))

(defun sfs--handle-message (ws frame)
  "Handle FRAME sent over WS."
  (declare (ignore ws))
  (let ((msg (websocket-frame-text frame)))
    (message "GOT MSG: %s" msg)))

(defvar sfs--server nil)

(defun sfs--start-server ()
  "Start websockets server."
  (setf sfs--clients nil)

  (when (not (eq (process-status sfs--server) 'closed))
    (websocket-server-close sfs--server))

  (setf sfs--server
        (websocket-server
         sfs--server-port
         :host 'local
         :on-open #'sfs--handle-client-open
         :on-close #'sfs--handle-client-close
         :on-message #'sfs--handle-message
         :on-error #'sfs--handle-server-error)))

(defun sfs--send (msg)
  "Send MSG to all connected clients."
  (cl-dolist (ws sfs--clients)
    (websocket-send-text ws msg)))

(provide 'spookfox-server)
;;; spookfox-server.el ends here
