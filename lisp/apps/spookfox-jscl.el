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

;; TODO Figure out a better name for this function
;;
;; There are 3 evaluation contexts for a browser extension:
;; 1. The background script; which can be considered *the addon* itself
;; 2. The content script; which runs inside a web-page e.g on bitspook.in
;; 3. The popup(?) script; which runs in addon's popup-ui page
;;
;; I intend to support running lisp in all 3 of them, so we need a good name.
;; Here's the best I could come up with in midst of excitement of getting this
;; thing working. Perhaps you can come up with a better name?
(cl-defun sfcl-eval (form &optional (context 'background))
  "Evaluate LISP FORM in background script in CONTEXT.
CONTEXT can be one of '(background ).

Note: JSCL uses #j: for FFI, but FORM must use `js:` for that,
because emacs-lisp do not allow writing #j: forms, even in quoted
form."
  (let ((client (cl-first spookfox--connected-clients))
        (str-form (string-replace "(js:" "(#j:" (prin1-to-string form))))
    (case context
      ('background
       (when client
         (plist-get
          (spookfox--poll-response (sfcl--request client "EVAL_BG" str-form))
          :payload)))
      (t (error "Unsupported context: %s" context)))))

(defun sfcl-js-obj (alist)
  "Create a javascript object from ALIST."
  `(js:Object:fromEntries
    (js:Array
     ,@(mapcar
        (lambda (cell) `(js:Array ,(car cell) ,(cdr cell)))
        alist))))

;;;###autoload
(defun spookfox-jscl ()
  "Initialize spookfox-jscl app."
  ;; Pass. Nothing to initialize.
  )

(provide 'spookfox-jscl)
;;; spookfox-jscl.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("sfcl-" . "spookfox-jscl-"))
;; End:
