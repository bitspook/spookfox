;;; spookfox-tabs -- Spookfox app which provide access to browser's tabs -*- lexical-binding: t -*-

;;; Commentary:
;; Access browser tabs from Emacs

;;; Code:
(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'spookfox)

(defvar spookfox-tabs--msg-prefix "T_")

(defun spookfox-tabs--request (&rest args)
  "Make spookfox-request with CLIENT and ARGS but with prefixed NAME."
  (let ((spookfox--msg-prefix spookfox-tabs--msg-prefix))
    (apply #'spookfox-request args)))

(defun spookfox-request-active-tab ()
  "Get details of active tab in browser."
  (let ((client (cl-first spookfox--connected-clients)))
    (when client
      (plist-get
       (spookfox--poll-response (spookfox-tabs--request client "GET_ACTIVE_TAB"))
       :payload))))

(defun spookfox--request-all-tabs ()
  "Get all tabs currently present in browser."
  (let ((client (cl-first spookfox--connected-clients)))
    (when client
      (spookfox-tabs--request client "GET_ALL_TABS")
      (plist-get (spookfox--poll-last-msg-payload) :payload))))

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

;;;###autoload
(defun spookfox-tabs ()
  "Initialize spookfox-tabs app."
  ;; Pass. Nothing to initialize.
  )

(provide 'spookfox-tabs)
;;; spookfox-tabs.el ends here
