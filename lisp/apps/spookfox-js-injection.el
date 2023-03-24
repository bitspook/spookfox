;;; spookfox-js-injection -- Spookfox app to inject Javascript into browser tabs -*- lexical-binding: t -*-

;;; Commentary:
;; Access, save and manipulate browser tabs

;;; Code:
(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'spookfox)

(defvar sfjsi--msg-prefix "JS_INJECT_")

(defun sfjsi--request (&rest args)
  "Make spookfox-request with CLIENT and ARGS but with prefixed NAME."
  (let ((spookfox--msg-prefix sfjsi--msg-prefix))
    (apply #'spookfox-request args)))

(defun spookfox-eval-js-in-active-tab (js &optional just-the-tip-p)
  "Evaluate JS in active firefox tab.
Return value is a list of lists. Browser can have multiple active
tabs (one per window). Every active tab can have multiple frames.
If JUST-THE-TIP-P is non-nil, first tab's first frame's return
value from the results is returned (instead of list of lists).

JS is subjected to limitations of browser's ability to execute
it. It is similar to executing js in browser's console. So for
example running a script which declares a variable with `let` or
`const` might cause the script to fail execution.

Details about js execution:
https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/API/tabs/executeScript"
  (let ((client (first spookfox--connected-clients)))
    (when client
      (let ((result (plist-get
                     (spookfox--poll-response
                      (sfjsi--request
                       client "EVAL_IN_ACTIVE_TAB"
                       `((code . ,js))))
                     :payload)))
        (if just-the-tip-p (seq-first (seq-first result))
          result)))))

(cl-defun sfjsi-eval (js &optional (context 'background))
  "Evaluate JS in CONTEXT.
Supported contexts: '(background)."
  (let ((client (first spookfox--connected-clients)))
    (when client
      (plist-get
       (spookfox--poll-response
        (case context
          ('background
           (sfjsi--request client "EVAL_IN_BACKGROUND_SCRIPT" `((code . ,js))))
          (t (error "Unsupported context: %s" context))))
       :payload))))

;;;###autoload
(defun spookfox-js-injection ()
  "Initialize spookfox-js-injection app."
  )

(provide 'spookfox-js-injection)
;;; spookfox-js-injection.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("sfjsi-" . "spookfox-js-injection-"))
;; End:
