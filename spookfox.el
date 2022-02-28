;;; package --- Summary -*- lexical-binding: t -*-
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
(require 'cl-lib)
(require 'json)
(require 'org-capture)

(defvar sf--connection nil
  "Connection to spookfox socket.")

(defvar sf--last-action-output nil
  "Output produced by last spookfox action.")

(defvar spookfox-saved-tabs-target '(file+headline (expand-file-name "spookfox.org" org-directory) "Tabs")
  "Target parse-able by org-capture-template where browser tabs will be saved.")

(defvar sf--tab-history nil
  "History of accessing spookfox tabs.")

(defvar sf--tab-group-history nil
  "History of accessing spookfox tab groups.")

(defvar sf--actions-alist nil
  "A mapping of spookfox actions to functions.
Alist representing functions to call when an action from spookfox
browser is received.")

(defun sf--exec-action (action)
  "Execute ACTION sent from spookfox browser."
  (cl-dolist (executioner (cdr (assoc (plist-get action :action) sf--actions-alist #'string=)))
    (funcall executioner action)))

(defun sf--add-action (action executioner)
  "Run EXECUTIONER every time ACTION is received from browser."
  (let ((cell (assoc action sf--actions-alist #'string=)))
    (if (not cell)
        (push (cons action (list executioner)) sf--actions-alist)
      (push executioner (cdr cell)))))

(defun sf--process-output-filter (_process output)
  "Save OUTPUT of last action sent to spookfox PROCESS.
For now let's only keep track of the last message. If it the
communication b/w browser and Emacs gets noisier, we'll introduce
a request-id system to keep track of requests and their
responses, and maintain a data structure."
  (message "Spookfox message: %s"  output)
  (let ((msg (json-parse-string
              (plist-get (json-parse-string output :object-type 'plist) :payload)
              :object-type 'plist)))
    (if (plist-get msg :action)
        (sf--exec-action msg)
      (setq sf--last-action-output msg))))

(defun sf--connect ()
  "Connect or re-connect to spookfox browser addon."
  (when (or (not sf--connection) (eq (process-status sf--connection) 'closed))
    (setq sf--connection (make-network-process
                          :name "spookfox"
                          :buffer "*spookfox*"
                          :family 'local
                          :remote "/tmp/spookfox.socket"))
    (set-process-filter sf--connection #'sf--process-output-filter)))

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

(defun sf--poll-last-msg-payload (&optional call-count)
  "Synchronously provide latest message received from browser.
Returns a plist obtained be decoding incoming message. Since
socket-communication with spookfox is async, this function blocks
Emacs for 1 second. If it don't receive a response in that time,
it returns `nil`. CALL-COUNT is for internal use, for reaching
exit condition in recursive re-checks."
  (cl-block sf--poll-last-msg-payload
    (let ((msg sf--last-action-output)
          (call-count (or call-count 0)))
      (when (> call-count 5)
        (cl-return-from sf--poll-last-msg-payload))
      (when (not msg)
        (sleep-for 0 200)
        (cl-return-from sf--poll-last-msg-payload (sf--poll-last-msg-payload (1+ call-count))))
      (setq sf--last-action-output nil)
      msg)))

(defun sf--send-action (action &optional payload)
  "Utility to send ACTION type messages, and optionally a PAYLOAD."
  (sf--send-message `((action . ,action)
                      (payload . ,payload))))

(defun sf--request-active-tab ()
  "Get details of active tab in browser."
  (sf--send-action "GET_ACTIVE_TAB")
  (sf--poll-last-msg-payload))

(defun sf--request-all-tabs ()
  "Get all tabs currently present in browser."
  (sf--send-action "GET_ALL_TABS")
  (sf--poll-last-msg-payload))

(defun sf--serialize-tab (tab)
  "Convert browser TAB to org-mode-subtree string."
  (concat "* "
          (plist-get tab :title)
          "\n:PROPERTIES:\n"
          ":TAB_ID:\t" (format "%d" (plist-get tab :id)) "\n"
          ":URL:\t" (plist-get tab :url) "\n"
          ":END:\n"))

(defun sf--deserialize-tab ()
  "Return browser tab for subtree at point.
This function is useful for `org-map-entries`."
  (let ((props (org-entry-properties)))
    `(:title ,(alist-get "ITEM" props nil nil #'string=)
      :url ,(alist-get "URL" props nil nil #'string=)
      :tabId ,(alist-get "TAB_ID" props nil nil #'string=)
      :tags ,(mapcar #'substring-no-properties (org-get-tags)))))

(defun sf--textify-plist (pl title-prop)
  "Convert PL plist to text obtained by TITLE-PROP property.

All the plist's properties are put on the resultant text as
text-properties."
  (let ((text (plist-get pl title-prop)))
    (cl-dolist (p pl)
      (when (and (keywordp p) (not (eq title-prop p)))
        (put-text-property 0 1 p (plist-get pl p) text)))
    text))

(defun sf--save-tabs (tabs)
  "Save spookfox TABS as an `org-mode` subtree.
Tabs subtree is saved in `spokfox-saved-tabs-target`"
  (let* ((tabs-subtree (seq-mapcat #'sf--serialize-tab tabs 'string)))
    (org-capture-set-target-location spookfox-saved-tabs-target)
    (org-capture-put :template tabs-subtree)
    (org-capture-place-template)))

(defun sf--get-saved-tabs ()
  "Get browser tabs saved with spookfox.
Returns a list of tabs as plists. Any subtree which don't have a
TAB_ID is discarded."
  (let ((tabs nil))
    (org-capture-set-target-location spookfox-saved-tabs-target)
    (save-excursion
      (with-current-buffer (org-capture-get :buffer)
        (goto-char (org-capture-get :pos))
        (org-narrow-to-subtree)
        (setq tabs (org-map-entries #'sf--deserialize-tab))
        (widen)))
    (seq-filter (lambda (tab) (plist-get tab :tabId)) tabs)))

(defun sf--tab-read ()
  "Ask user to select a tab using Emacs' completion system."
  (let* ((tabs (mapcar
                (lambda (pl)
                  (cons
                   (concat (plist-get pl :title) "\t\t(" (plist-get pl :url) ")" "[" (plist-get pl :tabId) "]")
                   pl))
                (sf--get-saved-tabs)))
         (annotation-function nil)
         (tab (completing-read
               "Open tab in browser: "
               (lambda (string pred action)
                 (if (eq action 'metadata)
                     `(metadata
                       (display-sort-function . identity)
                       (cycle-sort-function . identity)
                       (annotation-function . ,annotation-function)
                       (category . spookfox-tab))
                   (complete-with-action action tabs string pred)))
               nil nil nil 'sf--tab-history)))
    (or (cdr (assoc tab tabs))
        tab)))

(defun sf--tab-p (tab)
  "Return t if TAB is a spookfox tab, nil otherwise."
  (when (plist-get tab :tabId) t))

;; Public interface
(defun spookfox-save-all-tabs ()
  "Save all currently open browser tabs at `spookfox-saved-tabs-target`.
It will open a capture buffer so user get a chance to preview and
make changes."
  (interactive)
  (let ((tabs (sf--request-all-tabs)))
    (sf--save-tabs tabs)))

(defun spookfox-save-active-tab ()
  "Save active tab in browser."
  (interactive)
  (let ((tab (sf--request-active-tab)))
    (sf--save-tabs (list tab))))

(defun spookfox-open-tab ()
  "Prompt user to select a tab and open it in spookfox browser."
  (interactive)
  (let ((tab (sf--tab-read)))
    (cond
     ((sf--tab-p tab)
      (sf--send-action "OPEN_TAB" tab))
     ((string-match "https?:\/\/.*[\.].*" tab)
      (sf--send-action "OPEN_TAB" `(:url ,tab)))
     (t
      (sf--send-action "SEARCH_FOR" tab)))))

(defun spookfox-open-tab-group ()
  "Prompt user to select a tab group, and open all tabs in it."
  (interactive)
  (let* ((tabs (sf--get-saved-tabs))
         (groups (seq-uniq (seq-mapcat (lambda (tab) (plist-get tab :tags)) tabs)))
         (selected-group (completing-read "Select tab group: " groups nil t nil sf--tab-group-history))
         (group-tabs (seq-filter (lambda (tab) (seq-contains-p (plist-get tab :tags) selected-group #'string=)) tabs)))
    (sf--send-action
     "OPEN_TABS"
     (json-parse-string                 ; json-encode kinda messes up converting list
                                        ; of plists; so we make proper
                                        ; json-string, parses it to hashmap so
                                        ; sf--send-action can parse it again
                                        ; into a proper JSON array
      (concat "[" (string-join (mapcar #'json-encode group-tabs) ",") "]")))))

(provide 'spookfox)
;;; spookfox.el ends here
