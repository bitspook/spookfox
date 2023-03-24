;;; spookfox-tabs -- Spookfox app which provide access to browser's tabs -*- lexical-binding: t -*-

;;; Commentary:
;; Access browser tabs from Emacs

;;; Code:
(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'spookfox)

(defvar sfcl--msg-prefix "JSCL_")

(defun sfcl--request (&rest args)
  "Make spookfox-request with CLIENT and ARGS but with prefixed NAME."
  (let ((spookfox--msg-prefix sfcl--msg-prefix))
    (apply #'spookfox-request args)))

(defun sfcl-eval-in-bg (form)
  "Evaluate LISP FORM in background script."
  (let ((client (cl-first spookfox--connected-clients))
        (str-form (prin1-to-string form)))
    (when client
      (plist-get
       (spookfox--poll-response (sfcl--request client "EVAL_BG" str-form))
       :payload))))

;;;###autoload
(defun spookfox-jscl ()
  "Initialize spookfox-jscl app."
  ;; Pass. Nothing to initialize.
  )

(provide 'spookfox-tabs)
;;; spookfox-jscl.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("sfcl-" . "spookfox-jscl-"))
;; End:
