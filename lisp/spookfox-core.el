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

(defvar sf--last-response nil
  "Most recently received response from browser.")

(defvar sf--req-handlers-alist nil
  "A mapping of spookfox requests and their handlers.")

(defvar sf--last-faulty-pkt nil
  "Last Packet which caused a json encoding error. Useful for debugging.")

(defun spookfox--send-message (msg)
  "Send a MSG to browsers connected with spookfox.
MSG will be JSON encoded before sending."
  (sfs--send (json-encode msg)))

(defun spookfox-request (name &optional payload)
  "Make a request with NAME and optionally a PAYLOAD to browser.
Returns the request-id so the caller can retrieve a response
corresponding to this request."
  (let ((id (org-id-uuid)))
    (spookfox--send-message `((name . ,name)
                              (id . ,id)
                              (payload . ,payload)))
    id))

(defun spookfox--poll-last-msg-payload (&optional retry-count)
  "Synchronously provide latest message received from browser.
Returns a plist obtained be decoding incoming message. Since
socket-communication with spookfox is async, this function blocks
Emacs for maximum 1 second. If it don't receive a response in
that time, it returns `nil`. RETRY-COUNT is for internal use, for
reaching exit condition in recursive re-checks."
  (cl-block spookfox--poll-last-msg-payload
    (let ((msg sf--last-response)
          (retry-count (or retry-count 0)))
      (when (> retry-count 5)
        (cl-return-from spookfox--poll-last-msg-payload))
      (when (not msg)
        (sleep-for 0 200)
        (cl-return-from spookfox--poll-last-msg-payload (spookfox--poll-last-msg-payload (1+ retry-count))))
      (setq sf--last-response nil)
      msg)))

(defun sf--process-output-filter (_process pkt-str)
  "Choose what to do with PKT-STR sent by spookfox-native.
It try to convert the packet to JSON, and pass it to
request-handler if it is a request. Otherwise, it is a response.
For now it keep track of only the last response . If the
communication b/w browser and Emacs gets noisier, we'll do
something with the request-ids to keep track of requests and their
responses."
  (with-current-buffer "*spookfox*"
    (goto-char (point-max))
    (insert pkt-str))

  (let* ((pkt (condition-case nil
                  (json-parse-string pkt-str :object-type 'plist)
                (error (setf sf--last-faulty-pkt pkt-str)
                       (message "Spookfox failed to parse a packet. Check `sf--last-faulty-pkt`")
                       nil)))
         (msg (plist-get pkt :message)))
    (when msg
      (if (plist-get msg :name)
          (spookfox--handle-request msg)
        (setq sf--last-response msg)))))

(defun spookfox--handle-request (request)
  "Handle REQUEST sent from browser."
  (let* ((handler (cdr (assoc (plist-get request :name) sf--req-handlers-alist #'string=)))
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
         (res-payload (funcall handler req-payload)))
    (spookfox--send-message `(:requestId ,request-id :payload ,res-payload))))

(defun spookfox--register-req-handler (request handler)
  "Run HANDLER every time REQUEST is received from browser.
Return value of HANDLER is sent back to browser as response."
  (let ((cell (assoc request sf--req-handlers-alist #'string=)))
    (when cell (error "Handler already registered. There can only by one handler per request"))
    (push (cons request handler) sf--req-handlers-alist)))

(provide 'spookfox-core)
;;; spookfox-core.el ends here
