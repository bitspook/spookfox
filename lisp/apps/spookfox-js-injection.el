;;; spookfox-js-injection -- Spookfox app to inject Javascript into browser tabs -*- lexical-binding: t -*-

;;; Commentary:
;; Access, save and manipulate browser tabs

;;; Code:
(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'spookfox)
(require 'spookfox-tabs)

(defvar sfjsi--msg-prefix "JS_INJECT_")

(defun sfjsi--request (&rest args)
  "Make spookfox-request with CLIENT and ARGS but with prefixed NAME."
  (let ((spookfox--msg-prefix sfjsi--msg-prefix))
    (apply #'spookfox-request args)))

(defun sfjsi-eval-in-active-tab (js &optional just-the-tip-p)
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
  (let ((client (cl-first spookfox--connected-clients)))
    (when client
      (let ((result (plist-get
                     (spookfox--poll-response
                      (sfjsi--request
                       client "EVAL_IN_ACTIVE_TAB"
                       `((code . ,js))))
                     :payload)))
        (if just-the-tip-p (seq-first (seq-first result))
          result)))))

(defalias 'spookfox-eval-js-in-active-tab 'sfjsi-eval-in-active-tab)
(make-obsolete 'spookfox-eval-js-in-active-tab 'sfjsi-eval-in-active-tab 'v0.3.0)

(defun sfjsi--eval-in-tabs (client js select-tab-p)
  "Eval JS in all tabs for which SELECT-TAB-P return non nil.
CLIENT is the websocket client. Return list of (:tab :result)
with result of execution from each selected tab."
  (let* ((all-tabs (spookfox--request-all-tabs))
         (selected-tabs (seq-filter
                         (lambda (tab) (funcall select-tab-p tab))
                         all-tabs)))
    (mapcar
     (lambda (tab)
       (list :tab tab
             :result (plist-get
                      (spookfox--poll-response
                       (sfjsi--request client "EVAL_IN_TAB"
                                       `((code . ,js) (tab-id . ,(plist-get tab :id)))))
                      :payload)))
     selected-tabs)))

;;;###autoload
(cl-defun sfjsi-eval (js &optional (context 'background) (select-tab-p nil))
  "Evaluate JS in CONTEXT. Return the result of evaluation.

Supported contexts:

1. background
   Eval JS in addon's background page.
2. tab
   Eval JS in tab. When non-nil SELECT-TAB-P should be a function which
   receives each tab (a plist of at least :url, :id, :title, :windowId),
   and JS is evaluated in every tab for which it returns non-nil.
"
  (let ((client (cl-first spookfox--connected-clients)))
    (when client
      (cl-case context
        (background (plist-get
                     (spookfox--poll-response
                      (sfjsi--request client "EVAL_IN_BACKGROUND_SCRIPT" `((code . ,js))))
                     :payload))
        (tab (sfjsi--eval-in-tabs client js select-tab-p))
        (t (error "Unsupported context: %s" context))))))

(provide 'spookfox-js-injection)
;;; spookfox-js-injection.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("sfjsi-" . "spookfox-js-injection-"))
;; End:
