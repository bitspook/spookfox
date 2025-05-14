;;; spookfox-windows -- Spookfox app which provide access to browser's windows -*- lexical-binding: t -*-

;;; Commentary:
;; Access browser windows from Emacs

;;; Code:
(require 'spookfox)

(defvar sfw--msg-prefix "WINDOWS")

(defun sfw--request-all-windows ()
  "Get all tabs currently present in browser."
  (let ((windows nil))
    (dolist (client spookfox--connected-clients windows)
      (when client
        (push (plist-get
               (spookfox--poll-response
                (spookfox-request client (format "%s_GET_ALL" sfw--msg-prefix)))
               :payload)
              windows)))))

(provide 'spookfox-windows)
;;; spookfox-windows.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("sfw-" . "spookfox-windows-"))
;; End:
