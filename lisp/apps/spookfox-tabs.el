;;; spookfox-tabs -- Spookfox app which provide access to browser's tabs -*- lexical-binding: t -*-

;;; Commentary:
;; Access browser tabs from Emacs

;;; Code:
(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'spookfox)
(require 'spookfox-jscl)
(require 'spookfox-windows)

(defvar spookfox-tabs--msg-prefix "T_")

(defun spookfox-tabs--request (&rest args)
  "Make spookfox-request with CLIENT and ARGS but with prefixed NAME."
  (let ((spookfox--msg-prefix spookfox-tabs--msg-prefix))
    (apply #'spookfox-request args)))

(defun spookfox-request-active-tab (&optional window-id)
  "Get details of active tab in browser. Optionally provide a numeric WINDOW-ID."
  (let ((client (cl-first spookfox--connected-clients)))
    (when client
      (plist-get
       (spookfox--poll-response (spookfox-tabs--request client "GET_ACTIVE_TAB" `(:windowId ,window-id)))
       :payload))))

(defun spookfox--request-all-tabs ()
  "Get all tabs currently present in browser."
  (let ((client (cl-first spookfox--connected-clients)))
    (when client
      (plist-get
       (spookfox--poll-response
        (spookfox-tabs--request client "GET_ALL_TABS"))
       :payload))))

(defun sft--open-or-search (term)
  "Open new tab with TERM."
  (let ((client (cl-first spookfox--connected-clients)))
    (when client
      (cond
       ((string-match "https?:\/\/.*[\.].*" term)
        (spookfox-tabs--request client "OPEN_TAB" `(:url ,term)))
       (t
        (spookfox-tabs--request client "SEARCH_FOR" term))))))

(defun spookfox-switch-tab ()
  "Like `switch-buffer' but for browser tabs.
When you have too many tabs to find what you want; or you want to
jump to browser with your desired tab already in focus. Or to open a new tab.

Note that this do not bring the browser window to focus.
Depending on the kind of system, user have to do it by themselves.
[[https://github.com/bitspook/spookmax.d/blob/aae6c47e5def0f2bc113f22931ec27c62b5365b6/readme.org?plain=1#L1757-L1764][Example]]"
  (interactive)
  (let* ((tabs (spookfox--request-all-tabs))
         (tabs (mapcar (lambda (tab)
                         (cons (concat (plist-get tab :title) "\t"
                                       (propertize (plist-get tab :url) 'face 'font-lock-comment-face))
                               tab))
                       tabs))
         (read-tab (completing-read "Select tab: " tabs))
         (selected-tab (alist-get read-tab tabs nil nil #'string=))
         (client (cl-first spookfox--connected-clients)))
    (if selected-tab
        (let ((tab-id (plist-get selected-tab :id))
              (window-id (plist-get selected-tab :windowId)))
          ;; (sfjsi-eval (format "browser.tabs.update(%s, { active: true });browser.windows.update(%s, { focused: true });" tab-id window-id))
          (sfcl-eval
           `(progn
              (js:browser:tabs:update ,tab-id ,(sfcl-js-obj '(("active" . t))))
              (js:browser:windows:update ,window-id ,(sfcl-js-obj '(("focused" . t))))
              t)))
      (sft--open-or-search read-tab))))

;;;###autoload
(defvar spookfox-tabs
  `(:name spookfox-tabs
    :dependencies (,spookfox-jscl ,spookfox-windows)))

(provide 'spookfox-tabs)
;;; spookfox-tabs.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("sft-" . "spookfox-tabs-") ("sfcl-" . "spookfox-jscl-") ("sfjsi-" . "spookfox-js-injection-"))
;; End:
