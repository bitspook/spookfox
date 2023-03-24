;;; spookfox-tabs -- Spookfox app which provide access to browser's tabs -*- lexical-binding: t -*-

;;; Commentary:
;; Access browser tabs from Emacs

;;; Code:
(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'spookfox)
(require 'spookfox-jscl)

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

(defun spookfox-open-new-tab ()
  "Prompt user to select a tab and open it in spookfox browser."
  (interactive)
  (let ((tab (read-string "Enter URL to open or search term: " "https://"))
        (client (cl-first spookfox--connected-clients)))
    (when client
      (cond
       ((string-match "https?:\/\/.*[\.].*" tab)
        (spookfox-tabs--request client "OPEN_TAB" `(:url ,tab)))
       (t
        (spookfox-tabs--request client "SEARCH_FOR" tab))))))

(defun spookfox-switch-tab ()
  "Like `switch-buffer' but for browser and its tabs.
When you have too many tabs to find what you want; or you want to
jump to browser with your desired tab already in focus."
  (interactive)
  (let* ((tabs (spookfox--request-all-tabs))
         (tabs (mapcar (lambda (tab)
                         (cons (concat (plist-get tab :title) "\t"
                                       (propertize (plist-get tab :url) 'face 'font-lock-comment-face))
                               tab))
                       tabs))
         (selected-tab (completing-read "Select tab: " tabs))
         (selected-tab (alist-get selected-tab tabs nil nil #'string=))
         (tab-id (plist-get selected-tab :id))
         (window-id (plist-get selected-tab :windowId))
         (client (cl-first spookfox--connected-clients)))
    ;; (sfjsi-eval (format "browser.tabs.update(%s, { active: true });browser.windows.update(%s, { focused: true });" tab-id window-id))
    (sfcl-eval
     `(progn
        (js:browser:tabs:update ,tab-id ,(sfcl-js-obj '(("active" . t))))
        (js:browser:windows:update ,window-id ,(sfcl-js-obj '(("focused" . t))))
        t))))

;;;###autoload
(defun spookfox-tabs ()
  "Initialize spookfox-tabs app."
  ;; We need spookfox-jscl to do some work
  (spookfox-jscl))

(provide 'spookfox-tabs)
;;; spookfox-tabs.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("sft-" . "spookfox-tabs-") ("sfcl-" . "spookfox-jscl-") ("sfjsi-" . "spookfox-js-injection-"))
;; End:
