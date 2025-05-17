;;; spookfox.el --- Communicate with a browser which have spookfox browser addon installed. -*- lexical-binding: t; -*-
;;
;; Copyright © 2022 bitspook
;;
;; Author: bitspook
;; Homepage: https://bitspook.in/projects/spookfox
;; Keywords: Firefox
;; Version: 0.7.1
;; Package-Requires: ((websocket "1.13"))
;;
;;; Commentary:
;;
;; Spookfox provides means to communicate with your browser. It is (or should
;; be, after you write some code) capable of doing everything which the browser
;; allows its extensions to do.
;;
;; Please read the readme.org file in this repository for details.
;;
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-capture)
(require 'org-id)
(require 'websocket)

(defvar spookfox-version "0.7.1"
  "Spookfox version.")
(defvar spookfox--responses nil
  "Alist of responses received. Key is the request-id, val is the response.")
(defvar spookfox--req-handlers-alist nil
  "A mapping of spookfox requests and their handlers.")
(defvar spookfox--last-faulty-msg nil
  "Last Packet which caused a json encoding error. Useful for debugging.")
(defvar spookfox-server--port 59001)
(defvar spookfox--connected-clients nil)
(defvar spookfox--server-process nil)
(defvar spookfox--msg-prefix ""
  "String to prefix names of messages sent by `spookfox-request'.")
(defvar spookfox-debug nil
  "When non-nil, spookfox will log its communication in *spookfox* buffer.")

(defvar spookfox-client-connected-hook nil
  "Hook that gets called every time a new client connects to spookfox server.")

(defvar spookfox-client-disconnected-hook nil
  "Hook that gets called every time a client connected to spookfox server disconnects.")

;; lib
(defun spookfox--string-blank-p (str)
  "Return t if STR is blank.
Considers hard-space (ASCII 160) as space."
  (string-blank-p (string-replace (concat '(160)) "" str)))
;; lib ends here

(defun spookfox--log (msg &rest args)
  "Log a MSG formatted with ARGS to *spookfox* buffer."
  (when spookfox-debug
    (with-current-buffer (get-buffer-create "*spookfox*")
      (goto-char (point-max))
      (insert (apply #'format (concat "\n" msg) args)))))

(defun spookfox--handle-new-client (ws)
  "When a new client connects, save the connected websocket WS."
  (cl-pushnew ws spookfox--connected-clients)
  (run-hook-with-args 'spookfox-client-connected-hook ws)
  (spookfox--log "[CONNECTED] Total clients: %s" (length spookfox--connected-clients)))

(defun spookfox--handle-disconnect-client (ws)
  "When a client connection closes, remove the websocket WS from saved sockets."
  (setf spookfox--connected-clients (cl-remove-if (lambda (saved-ws) (eq saved-ws ws)) spookfox--connected-clients))
  (run-hook-with-args 'spookfox-client-disconnected-hook ws)
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
request-handler if it is a request. Otherwise, it is treated as a
response. Response is saved in `spookfox--responses' alist with
request-id as key."
  (let ((msg (websocket-frame-text frame)))
    (spookfox--log "[MESSAGE] %s" msg)

    (let ((msg (condition-case nil
                   (json-parse-string msg :object-type 'plist)
                 (error (setf spookfox--last-faulty-msg msg)
                        (spookfox--log "[ERROR] Spookfox failed to parse a message. Check `spookfox--last-faulty-msg`")
                        nil))))
      (if (plist-get msg :name)
          (spookfox--handle-request msg)
        ;; FIXME There is a memory leak here. If a response is not polled for
        ;; (with `spookfox--poll-response', it is never removed from
        ;; `spookfox--responses'. We should implement a fixed-length data
        ;; structure; so even if nobody polls for a response, old responses
        ;; don't just keep lying around in `spookfox--responses'
        (push (cons (plist-get msg :requestId) msg) spookfox--responses)))))

;;;###autoload
(defun spookfox-start-server ()
  "Start websockets server."
  (interactive)
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

(defun spookfox-stop-server ()
  "Stop websockets server."
  (websocket-server-close spookfox--server-process))

(defun spookfox-request (client-ws name &optional payload)
  "Make a request to CLIENT-WS with NAME and optionally a PAYLOAD to browser.
Returns the request-id so the caller can retrieve a response
corresponding to this request."
  (let ((id (org-id-uuid))
        (name (concat
               spookfox--msg-prefix
               (if (symbolp name)
                   (string-replace "-" "_" (upcase (symbol-name name)))
                 (upcase name)))))
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

(defun spookfox--poll-response (request-id &optional retry-count)
  "Synchronously provide response for request with id REQUEST-ID.
Returns a plist obtained by decoding the response. Since
socket-communication with spookfox is async, this function blocks
Emacs for maximum 1 second. If it don't receive a response in
that time, it returns `nil`. RETRY-COUNT is for internal use, for
reaching exit condition in recursive re-checks."
  (cl-block spookfox--poll-response
    (let ((msg (cdr (assoc request-id spookfox--responses 'equal)))
          (retry-count (or retry-count 0)))
      (when (> retry-count 10)
        (cl-return-from spookfox--poll-response))
      (when (not msg)
        (sleep-for 0 50)
        (cl-return-from spookfox--poll-response (spookfox--poll-response request-id (1+ retry-count))))
      (setf spookfox--responses (delq (assoc request-id spookfox--responses 'equal) spookfox--responses))
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

(defun spookfox--register-req-handler (name handler)
  "Run HANDLER every time request with NAME is received from browser.
Return value of HANDLER is sent back to browser as response."
  (let* ((name (concat spookfox--msg-prefix name))
         (cell (assoc name spookfox--req-handlers-alist #'string=)))
    (when cell (warn "Handler already registered. Overwriting previously registered handler."))
    (push (cons name handler) spookfox--req-handlers-alist)))

;;;###autoload
(defun spookfox-init ()
  "Initialize spookfox.
This function is obsolete. Please use spookfox-start-server."
  (spookfox-start-server))
(make-obsolete #'spookfox-init #'spookfox-start-server 'v0.6.0)

(defun spookfox-shutdown ()
  "Stop spookfox."
  (interactive)
  (spookfox-stop-server))

(provide 'spookfox)
;;; spookfox.el ends here
