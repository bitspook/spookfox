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
(require 'org-id)

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

(defvar sf--known-tab-props '("tab_id" "url" "chained" "id")
  "List of properties which are read when an org node is converted to a tab.")

(defvar sf--reconnect-timer nil)

(defun sf--process-output-filter (_process pkt-str)
  "Choose what to do with PKT-STR sent by spookfox-native.
It try to convert the packet to JSON, and pass it to action
executioner if it is an action. Otherwise, for now it keep track
of the last message only. If the communication b/w browser and
Emacs gets noisier, we'll do something with the action-ids to
keep track of actions and their responses."
  (with-current-buffer "*spookfox*" (insert pkt-str))
  (let ((msg (mapcar
              (lambda (x)
                (cond
                 ;; because parsing JSON string to plist convert JS true/false
                 ;; to keywords
                 ((eq x :false) nil)
                 ((eq x :true) t)
                 (t x)))
              (plist-get (json-parse-string pkt-str :object-type 'plist) :message))))
    (if (plist-get msg :action)
        (sf--exec-action msg)
      (setq sf--last-action-output msg))))

(defun sf--process-sentinel (_process event)
  "Start a timer when break-connection EVENT is received."
  (message (format "Spookfox process had the event: %s" event))
  (spookfox-ensure-connection))

(defun sf--is-disconnected ()
  "Return t if connection to spookfox-native is not established."
  (or (not sf--connection) (eq (process-status sf--connection) 'closed)))

(defun sf--connect ()
  "Connect or re-connect to spookfox browser addon."
  (when (sf--is-disconnected)
    (setq sf--connection (make-network-process
                          :name "spookfox"
                          :buffer "*spookfox*"
                          :family 'local
                          :remote "/tmp/spookfox.socket"))
    (set-process-filter sf--connection #'sf--process-output-filter)
    (set-process-sentinel sf--connection #'sf--process-sentinel)
    (when sf--reconnect-timer
      (cancel-timer sf--reconnect-timer)
      (setq sf--reconnect-timer nil))
    (message "Connected to spookfox!")))

(defun spookfox-ensure-connection (&optional retry-count)
  "Make sure spookfox is connect at all times.
If spookfox connection is broken, try to re-connect every 10
seconds until connection is established. RETRY-COUNT tells how
many retry attempts have already been made, so next wait interval
is set accordingly."
  (when (and sf--reconnect-timer (not retry-count)) ;cancel the timer if explicitly called
    (cancel-timer sf--reconnect-timer)
    (setq sf--reconnect-timer nil))

  (ignore-errors (sf--connect))

  (let* ((retry-count (or retry-count 0))
         (retry-after (min (* retry-count 2) 15)))
    (when (sf--is-disconnected)
      (when (> retry-after 2) (message "Will retry spookfox connection after %ss" retry-after)) ;so we don't spam for momentary reconnects
      (setq sf--reconnect-timer (run-with-timer retry-after nil 'spookfox-ensure-connection (1+ retry-count))) )))

(defun sf--build-packet (msg)
  "Create a packet from MSG which can be sent over to spookfox-native."
  (concat (json-encode `((sender . "Emacs")
                         (status . "Success")
                         (message . ,(json-encode msg))))
          "\n"))

(defun sf--send-message (msg)
  "Send a MSG to browsers connected with spookfox.
MSG will be JSON encoded before sending. Throws error if
connection to spookfox-native is not established."
  (sf--connect)                         ;It's safe to call `sf--connect' since it is idempotent.
                                        ;This gives us a safety net in case the
                                        ;timer for re-connection is waiting
  (when (or
         (not sf--connection)
         (string= (process-status sf--connection) "closed"))
    (error "Not connected to spookfox-native"))
  (process-send-string sf--connection (sf--build-packet msg)))

(defun sf--send-action (action &optional payload)
  "Utility to send ACTION type messages, and optionally a PAYLOAD.
Returns the action-id."
  (let ((action-id (org-id-uuid)))
    (sf--send-message `((action . ,action)
                        (id . ,(org-id-uuid))
                        (payload . ,payload)))
    action-id))

(defun sf--poll-last-msg-payload (&optional retry-count)
  "Synchronously provide latest message received from browser.
Returns a plist obtained be decoding incoming message. Since
socket-communication with spookfox is async, this function blocks
Emacs for maximum 1 second. If it don't receive a response in
that time, it returns `nil`. RETRY-COUNT is for internal use, for
reaching exit condition in recursive re-checks."
  (cl-block sf--poll-last-msg-payload
    (let ((msg sf--last-action-output)
          (retry-count (or retry-count 0)))
      (when (> retry-count 5)
        (cl-return-from sf--poll-last-msg-payload))
      (when (not msg)
        (sleep-for 0 200)
        (cl-return-from sf--poll-last-msg-payload (sf--poll-last-msg-payload (1+ retry-count))))
      (setq sf--last-action-output nil)
      msg)))

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
          (seq-reduce (lambda (accum prop)
                        (when (and (keywordp prop) (not (seq-contains-p '(:title :tags) prop)))
                          (setq accum (concat (or accum "") (format "%s:\t" prop) (format "%s\n" (plist-get tab prop)))))
                        accum)
                      tab "")
          ":END:\n"))

(defun sf--deserialize-tab ()
  "Return browser tab for subtree at point.
This function is useful for `org-map-entries`."
  (let ((props (org-entry-properties)))
    `(:title ,(alist-get "ITEM" props nil nil #'string=)
             :tags ,(mapcar #'substring-no-properties (org-get-tags))
             ,@(seq-reduce (lambda (accum cell)
                             (when (seq-contains-p sf--known-tab-props (downcase (car cell)) #'string=) ;; org-mode upcase all the property names
                               (setq accum (plist-put accum (intern (downcase (format ":%s" (car cell)))) (cdr cell))))
                             accum)
                           props nil))))

(defun sf--textify-plist (pl title-prop)
  "Convert PL plist to text obtained by TITLE-PROP property.

All the plist's properties are put on the resultant text as
text-properties."
  (let ((text (plist-get pl title-prop)))
    (cl-dolist (p pl)
      (when (and (keywordp p) (not (eq title-prop p)))
        (put-text-property 0 1 p (plist-get pl p) text)))
    text))

(defun sf--save-tabs (tabs &optional hide-prompt?)
  "Save spookfox TABS as an `org-mode` subtree.
Tabs subtree is saved in `spokfox-saved-tabs-target`. Capture
buffer is not shown if HIDE-PROMPT? is non-nil."
  (let* ((tabs-subtree (seq-mapcat #'sf--serialize-tab tabs 'string))
         (org-capture-templates
          `(("t"
             "Spookfox tabs"
             entry
             ,spookfox-saved-tabs-target
             ,tabs-subtree
             :unnarrowed t
             :immidiate-finish ,(not hide-prompt?)))))
    (org-capture nil "t")))

(defmacro sf--with-tabs-subtree (&rest body)
  "Run BODY with current buffer set and narrowed to tabs org subtree.
Content of the `current-buffer' will be the complete tabs
subtree, not just the valid tabs. If you change `current-buffer',
you need to save it."
  `(let (res)
     (org-capture-set-target-location spookfox-saved-tabs-target)
     (save-excursion
       (with-current-buffer (org-capture-get :buffer)
         (goto-char (org-capture-get :pos))
         (org-narrow-to-subtree)
         (setq res (progn ,@body))
         (widen)))
     res))

(defun sf--get-saved-tabs ()
  "Get browser tabs saved with spookfox.
Returns a list of tabs as plists. Any subtree which don't have a
TAB_ID is discarded."
  (seq-filter
   #'sf--tab-p
   (sf--with-tabs-subtree
    (org-map-entries #'sf--deserialize-tab))))

(defun sf--update-tab (tab-id patch)
  "Update a saved tab matching TAB-ID with PATCH.
PATCH is a plist of properties to upsert."
  (sf--with-tabs-subtree
   (org-map-entries
    (lambda ()
      (when (string= (format "%s" tab-id) (org-entry-get (point) "TAB_ID"))
        (while patch
          (let ((prop (upcase (substring (symbol-name (pop patch)) 1)))
                (val (pop patch)))
            (cond
             ((string= "TAGS" prop)
              (org-set-tags val))
             ((string= "TITLE" prop)
              (org-edit-headline val))
             (t (org-entry-put (point) prop val)))))
        (save-buffer))))))

(defun sf--tab-read ()
  "Ask user to select a tab using Emacs' completion system."
  (let* ((tabs (mapcar
                (lambda (pl)
                  (cons
                   (concat (plist-get pl :title) "\t\t(" (plist-get pl :url) ")" "[" (plist-get pl :tab_id) "]")
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
  (when (plist-get tab :tab_id) t))

(defun sf--exec-action (action)
  "Execute ACTION sent from spookfox browser."
  (cl-dolist (executioner (cdr (assoc (plist-get action :action) sf--actions-alist #'string=)))
    (let ((action-id (or (plist-get action :id) "<unknown>"))
          (payload (funcall executioner action)))
      (sf--send-message `(:actionId ,action-id :payload ,payload)))))

(defun sf--add-action (action executioner)
  "Run EXECUTIONER every time ACTION is received from browser."
  (let ((cell (assoc action sf--actions-alist #'string=)))
    (when (or (not cell) (not (seq-contains-p (cdr cell) executioner)))
      (if (not cell)
          (push (cons action (list executioner)) sf--actions-alist)
        (push executioner (cdr cell))))))

(defun sf--handle-chain-tab (action)
  "Executioner for CHAIN_TAB ACTION."
  (let ((tab (plist-get action :payload)))
    (if tab
        (sf--update-tab (plist-get tab :tab_id) '(:chained "t"))
      (sf--save-tabs (list (plist-put tab :chained t))))))

(defun sf--handle-get-saved-tabs (_action)
  "Executioner for GET_SAVED_TABS ACTION."
  (concat "[" (string-join (mapcar #'json-encode (sf--get-saved-tabs)) ",") "]"))

(sf--add-action "CHAIN_TAB" #'sf--handle-chain-tab)
(sf--add-action "GET_SAVED_TABS" #'sf--handle-get-saved-tabs)

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

(spookfox-ensure-connection)

(provide 'spookfox)
;;; spookfox.el ends here
