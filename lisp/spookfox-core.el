;;; package -- Core functionality of spookfox -*- lexical-binding: t -*-

;;; Commentary:
;;; - Connecting to the browser
;;; - Communication with the browser

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-capture)
(require 'org-id)
(require 'websocket)
(require 'spookfox-lib)

(defvar spookfox-version "0.2.0"
  "Spookfox version.")
(defvar spookfox-available-apps nil)
(defvar spookfox-enabled-apps nil)
(defvar spookfox--last-response nil
  "Most recently received response from browser.")
(defvar spookfox--req-handlers-alist nil
  "A mapping of spookfox requests and their handlers.")
(defvar spookfox--last-faulty-msg nil
  "Last Packet which caused a json encoding error. Useful for debugging.")
(defvar spookfox-server--port 59001)
(defvar spookfox--connected-clients nil)
(defvar spookfox--server-process nil)

(defun spookfox--log (msg &rest args)
  "Log a MSG formatted with ARGS to *spookfox* buffer."
  (with-current-buffer (get-buffer-create "*spookfox*")
    (goto-char (point-max))
    (insert (apply #'format (concat "\n" msg) args))))

(defun spookfox--enable-app (app)
  "Register spookfox-app APP.
A spookfox APP is anything given there is a function named
spookfox--<app>-init, which is called to enable the app."
  (let* ((init-fn (alist-get app spookfox-available-apps)))
    (funcall init-fn)))

(defun spookfox--handle-new-client (ws)
  "When a new client connects, save the connected websocket WS."
  (push ws spookfox--connected-clients)
  (dolist (app spookfox-enabled-apps)
    (spookfox-request ws 'enable-app app))
  (spookfox--log "[CONNECTED] Total clients: %s" (length spookfox--connected-clients)))

(defun spookfox--handle-disconnect-client (ws)
  "When a client connection closes, remove the websocket WS from saved sockets."
  (setf spookfox--connected-clients (cl-remove-if (lambda (saved-ws) (eq saved-ws ws)) spookfox--connected-clients))
  (spookfox--log "[DISCONNECTED] Total clients: %s" (length spookfox--connected-clients)))

(defun spookfox--handle-server-error (_ws sym err)
  "Handle WS server error ERR in SYM callback."
  (warn "[spookfox-server] Error %s occurred in %s" err sym))

(defun spookfox--send-msg (msg &optional client-ws)
  "Send MSG to all connected clients.
If CLIENT-WS is provided, message is ent only to this client."
  (if client-ws (websocket-send-text client-ws msg)
    (cl-dolist (ws spookfox--connected-clients)
      (websocket-send-text ws msg))))

(defun spookfox--handle-msg (_ws frame)
  "Choose what to do with FRAME sent by a connected client.
It try to convert the FRAME text to JSON, and pass it to
request-handler if it is a request. Otherwise, it is treated as a response.
For now it keep track of only the last response . If the
communication b/w browser and Emacs gets noisier, we'll do
something with the request-ids to keep track of requests and their
responses."
  (let ((msg (websocket-frame-text frame)))
    (spookfox--log "[MESSAGE] %s" msg)

    (let ((msg (condition-case nil
                   (json-parse-string msg :object-type 'plist)
                 (error (setf spookfox--last-faulty-msg msg)
                        (spookfox--log "Spookfox failed to parse a message. Check `spookfox--last-faulty-msg`")
                        nil))))
      (if (plist-get msg :name)
          (spookfox--handle-request msg)
        (setq spookfox--last-response msg)))))

(defun spookfox-start-server ()
  "Start websockets server."
  (setf spookfox--connected-clients nil)

  (when (and spookfox--server-process
             (not (eq (process-status spookfox--server-process) 'closed)))
    (websocket-server-close spookfox--server-process))

  (setf spookfox--server-process
        (websocket-server
         spookfox-server--port
         :host 'local
         :on-open #'spookfox--handle-new-client
         :on-close #'spookfox--handle-disconnect-client
         :on-message #'spookfox--handle-msg
         :on-error #'spookfox--handle-server-error)))

(defun spookfox-request (client-ws name &optional payload)
  "Make a request to CLIENT-WS with NAME and optionally a PAYLOAD to browser.
Returns the request-id so the caller can retrieve a response
corresponding to this request."
  (let ((id (org-id-uuid))
        (name (if (symbolp name)
                  (string-replace "-" "_" (upcase (symbol-name name)))
                (upcase name))))
    (spookfox--send-msg
     (json-encode `((name . ,name)
                    (id . ,id)
                    (payload . ,payload)))
     client-ws)
    id))

(defun spookfox-request-all (name &optional payload)
  "Make a NAME request to all connected spookfox clients with optional PAYLOAD."
  (dolist (ws spookfox--connected-clients)
    (spookfox-request ws name payload)))

(defun spookfox--poll-last-msg-payload (&optional retry-count)
  "Synchronously provide latest message received from browser.
Returns a plist obtained be decoding incoming message. Since
socket-communication with spookfox is async, this function blocks
Emacs for maximum 1 second. If it don't receive a response in
that time, it returns `nil`. RETRY-COUNT is for internal use, for
reaching exit condition in recursive re-checks."
  (cl-block spookfox--poll-last-msg-payload
    (let ((msg spookfox--last-response)
          (retry-count (or retry-count 0)))
      (when (> retry-count 5)
        (cl-return-from spookfox--poll-last-msg-payload))
      (when (not msg)
        (sleep-for 0 200)
        (cl-return-from spookfox--poll-last-msg-payload (spookfox--poll-last-msg-payload (1+ retry-count))))
      (setq spookfox--last-response nil)
      msg)))

(defun spookfox--handle-request (request &optional client-ws)
  "Handle REQUEST sent from browser.
If CLIENT-WS is provided, response is sent to this client only."
  (let* ((request-name (plist-get request :name))
         (handler (cdr (assoc request-name spookfox--req-handlers-alist #'string=)))
         (request-id (or (plist-get request :id) "<unknown>"))
         (req-payload (mapcar
                       (lambda (x)
                         (pcase x
                           ;; because parsing JSON string to plist convert JS true/false
                           ;; to keywords :true/:false sometimes. Seems like a bug in Emacs's
                           ;; JSON parsing
                           (:false nil)
                           (:true t)
                           ("null" nil)
                           (val val)))
                       (plist-get request :payload)))
         (res-payload (if handler (funcall handler req-payload)
                        `((status . error)
                          (message . ,(format "Unknown request: %s" request-name))))))
    (spookfox--send-msg (json-encode `((requestId . ,request-id)
                                       (payload . ,res-payload)))
                        client-ws)))

(defun spookfox--register-req-handler (request handler)
  "Run HANDLER every time REQUEST is received from browser.
Return value of HANDLER is sent back to browser as response."
  (let ((cell (assoc request spookfox--req-handlers-alist #'string=)))
    (when cell (warn "Handler already registered. Overwriting previously registered handler."))
    (push (cons request handler) spookfox--req-handlers-alist)))

(provide 'spookfox-core)
;;; spookfox-core.el ends here
