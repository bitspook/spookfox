;;; package --- Summary
;;;
;;; Communicate with a browser which have spookfox browser addon installed.
;;
;;; Commentary:
;;;
;;; Spookfox provides means to communicate with your browser. It is (or should
;;; be, after you write some code) capable of doing everything which the browser
;;; allows its extensions to do.
;;;
;;; Please read the readme.org file in this repository for details.
;;;
;;; Code:
(defvar sf--connection nil
  "Connection to spookfox socket.")

(defun sf--connect ()
  "Connect or re-connect to spookfox browser addon."
  (setq sf--connection (make-network-process
                        :name "spookfox"
                        :buffer "*spookfox*"
                        :family 'local
                        :remote "/tmp/spookfox.socket")))

(defun sf--build-message (msg)
  "Create a message from MSG which can be sent over to spookfox browser addon."
  (concat (json-encode `((sender . "Emacs")
                         (type . "Success")
                         (payload . ,(json-encode msg))))
          "\n"))

(defun sf--send-message (msg)
  "Send a MSG to browsers connected with spookfox.

MSG will be json encoded before sending."
  (when (or
         (not sf--connection)
         (string= (process-status sf--connection) "closed"))
    (sf--connect))
  (process-send-string sf--connection (sf--build-message msg)))

(defun sf--get-last-message ()
  "Provide latest message received from browser connected with spookfox.

Returns a plist"
  (let ((msg-str
         (with-current-buffer (process-buffer sf--connection)
           (buffer-substring (progn
                               (goto-char (point-max))
                               (forward-line -1)
                               (point))
                             (point-max)))))
    (json-parse-string
     (plist-get (json-parse-string msg-str :object-type 'plist) :payload)
     :object-type 'plist)))

(defun sf--get-active-tab ()
  "Get details of active tab in browser."
  (sf--send-message '((type . "GET_ACTIVE_TAB")))
  (sf--get-last-message))

(provide 'spookfox)
;;; spookfox.el ends here
