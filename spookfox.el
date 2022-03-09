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
(require 'org)
(require 'org-capture)
(require 'org-id)

(defvar sf--connection nil
  "Connection to spookfox socket.")

(defvar sf--last-response nil
  "Most recently received response from browser.")

(defvar spookfox-saved-tabs-target '(file+headline (expand-file-name "spookfox.org" org-directory) "Tabs")
  "Target parse-able by org-capture-template where browser tabs will be saved.")

(defvar sf--tab-history nil
  "History of accessing spookfox tabs.")

(defvar sf--tab-group-history nil
  "History of accessing spookfox tab groups.")

(defvar sf--req-handlers-alist nil
  "A mapping of spookfox requests and their handlers.")

(defvar sf--known-tab-props '("id" "url" "chained" "id")
  "List of properties which are read when an org node is converted to a tab.")

(defvar sf--reconnect-timer nil)

(defvar spookfox-max-reconnect-retry-interval 30
  "Maximum number of seconds to wait if connection to browser fails.
We incrementally increase the time between reconnection attempts,
and max out at this value.")

(defvar sf--last-faulty-pkt nil
  "Last Packet which caused a json encoding error. Useful for debugging.")

;;; spookfox-lib
;;; Utility functions
(defun sf--string-banlk-p (str)
  "Return t if STR is blank.
Considers hard-space (ASCII 160) as space."
  (string-blank-p (string-replace (concat '(160)) "" str)))
;;; spookfox-lib ends here

;;; spookfox-core
;;; Core functionality of spookfox, regarding
;;; - Connecting to the browser
;;; - Communication with the browser
(defun sf--process-output-filter (_process pkt-str)
  "Choose what to do with PKT-STR sent by spookfox-native.
It try to convert the packet to JSON, and pass it to
request-handler if it is a request. Otherwise, it is a response.
For now it keep track of only the last response . If the
communication b/w browser and Emacs gets noisier, we'll do
something with the request-ids to keep track of requests and their
responses."
  (with-current-buffer "*spookfox*"
    (goto-char (point-max))
    (insert pkt-str))
  ;; Sometimes pkt-str contain more than one packets. I suppose that's just how
  ;; UNIX sockets work (they combine messages when there is congestion)? So we
  ;; have to manually split them at newlines
  (let ((pkt-strs (seq-filter (lambda (x) (not (string-empty-p x))) (split-string pkt-str "\n"))))
    (cl-dolist (pkt-str pkt-strs)
      (let* ((pkt (condition-case nil
                      (json-parse-string pkt-str :object-type 'plist)
                    (error (setq sf--last-faulty-pkt pkt-str)
                           (message "Spookfox failed to parse a packet. Check `sf--last-faulty-pkt`")
                           nil)))
             (msg (plist-get pkt :message)))
        (when msg
          (if (plist-get msg :name)
              (sf--handle-request msg)
            (setq sf--last-response msg)))))))

(defun sf--process-sentinel (_process event)
  "Start a timer when break-connection EVENT is received."
  (message (format "Spookfox process had the event: %s" event))
  (spookfox-ensure-connection))

(defun sf--connected-p ()
  "Return t if connection to spookfox-native is not established."
  (and sf--connection (not (eq (process-status sf--connection) 'closed))))

(defun sf--connect ()
  "Connect or re-connect to spookfox browser addon."
  (when (not (sf--connected-p))
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
    ;; Let's send a connected event to browser so it can refresh its state when
    ;; Emacs re-connects for whatever reason.
    (spookfox-request "CONNECTED")
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
    (when (not (sf--connected-p))
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
  (when (not (sf--connected-p))
    (error "Not connected to spookfox-native"))
  (process-send-string sf--connection (sf--build-packet msg)))

(defun spookfox-request (name &optional payload)
  "Make a request with NAME and optionally a PAYLOAD to browser.
Returns the request-id so the caller can retrieve a response
corresponding to this request."
  (let ((id (org-id-uuid)))
    (sf--send-message `((name . ,name)
                        (id . ,id)
                        (payload . ,payload)))
    id))

(defun sf--poll-last-msg-payload (&optional retry-count)
  "Synchronously provide latest message received from browser.
Returns a plist obtained be decoding incoming message. Since
socket-communication with spookfox is async, this function blocks
Emacs for maximum 1 second. If it don't receive a response in
that time, it returns `nil`. RETRY-COUNT is for internal use, for
reaching exit condition in recursive re-checks."
  (cl-block sf--poll-last-msg-payload
    (let ((msg sf--last-response)
          (retry-count (or retry-count 0)))
      (when (> retry-count 5)
        (cl-return-from sf--poll-last-msg-payload))
      (when (not msg)
        (sleep-for 0 200)
        (cl-return-from sf--poll-last-msg-payload (sf--poll-last-msg-payload (1+ retry-count))))
      (setq sf--last-response nil)
      msg)))
;;; spookfox-core ends here

;;; spookfox-tabs
;;; Access, save and manipulate browser tabs
(defun sf--request-active-tab ()
  "Get details of active tab in browser."
  (spookfox-request "GET_ACTIVE_TAB")
  (plist-get (sf--poll-last-msg-payload) :payload))

(defun sf--request-all-tabs ()
  "Get all tabs currently present in browser."
  (spookfox-request "GET_ALL_TABS")
  (plist-get (sf--poll-last-msg-payload) :payload))

(defun sf--insert-tab (tab)
  "Insert browser TAB as a new org-mode-subtree."
  (org-insert-heading)
  (let ((id (org-id-get-create)))
    (while tab
      (let ((prop (upcase (substring (format "%s" (pop tab)) 1)))
            (val (pop tab)))
        (cond
         ((string= "TITLE" prop) (org-edit-headline (if (sf--string-banlk-p val) "<empty-title>" val)))
         ((string= "TAGS" prop) (org-set-tags val))
         (t (org-entry-put (point) prop (format "%s" val))))))
    id))

(defun sf--deserialize-tab ()
  "Return spookfox tab for subtree at point.
This function is useful for `org-map-entries`."
  (let ((props (org-entry-properties)))
    `(:title ,(alist-get "ITEM" props nil nil #'string=)
      :tags ,(mapcar #'substring-no-properties (org-get-tags))
      ,@(seq-reduce (lambda (accum cell)
                      (when (seq-contains-p sf--known-tab-props (downcase (car cell)) #'string=) ;; org-mode upcase all the property names
                        (setq accum (plist-put accum
                                               (intern (downcase (format ":%s" (car cell))))
                                               (pcase (cdr cell)
                                                 ("t" t)
                                                 ("nil" nil)
                                                 (val val)))))
                      accum)
                    props nil))))

(defun sf--save-tabs (tabs &optional hide-prompt?)
  "Save spookfox TABS as an `org-mode` subtree.
Tabs subtree is saved in `spokfox-saved-tabs-target`. Capture
buffer is not shown if HIDE-PROMPT? is non-nil."
  (let* ((org-capture-templates
          `(("t"
             "Spookfox tabs"
             entry
             ,spookfox-saved-tabs-target
             "* %?"
             :unnarrowed t
             :immidiate-finish ,(not hide-prompt?)))))
    (org-capture nil "t")
    ;; Delete the "* " inserted by capture template; org-capture need us to
    ;; start with a valid org-entry, but `sf--insert-tab' adds its own entries
    ;; later.
    (delete-char -3)
    (let ((start-pos (point)))
      (seq-map (lambda (tab)
                 (sf--insert-tab tab)
                 (goto-char (point-max))) tabs)
      (goto-char start-pos))
    (recenter-top-bottom)))

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
ID and URL is discarded."
  (seq-filter
   #'sf--tab-p
   (sf--with-tabs-subtree
    (org-map-entries #'sf--deserialize-tab))))

(defun sf--find-tab-with-id (tab-id)
  "Find tab with TAB-ID."
  (sf--with-tabs-subtree
   (let ((pos (org-id-find-id-in-file tab-id (buffer-file-name))))
     (when pos
       (goto-char (cdr pos))
       (sf--deserialize-tab)))))

(defun sf--update-tab (tab-id patch)
  "Update a saved tab matching TAB-ID with PATCH.
PATCH is a plist of properties to upsert."
  (sf--with-tabs-subtree
   (let ((pos (org-id-find-id-in-file tab-id (buffer-file-name))))
     (when pos
       (goto-char (cdr pos))
       (while patch
         (let ((prop (upcase (substring (symbol-name (pop patch)) 1)))
               (val (pop patch)))
           (pcase prop
             ;; Empty titles have been observed in the wild
             ("TITLE" (org-edit-headline (if (sf--string-banlk-p val) "<empty-title>" val)))
             ("TAGS" (org-set-tags val))
             (_ (org-entry-put (point) prop val)))))
       (save-buffer)))))

(defun sf--remove-tab (tab-id)
  "Remove tab with TAB-ID."
  (sf--with-tabs-subtree
   (let ((pos (org-id-find-id-in-file tab-id (buffer-file-name))))
     (when pos
       (goto-char (cdr pos))
       (org-narrow-to-subtree)
       (delete-region (point-min) (point-max))
       (widen)
       (delete-line)
       (save-buffer)))))

(defun sf--tab-read ()
  "Ask user to select a tab using Emacs' completion system."
  (let* ((tabs (mapcar
                (lambda (pl)
                  (cons
                   (concat (plist-get pl :title) "\t\t(" (plist-get pl :url) ")" "[" (plist-get pl :id) "]")
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
  (when (and (plist-get tab :id) (plist-get tab :url)) t))
;;; spookfox-tabs ends here

;;; spookfox-request
;;; Code for receiving and replying to requests from browser.
(defun sf--handle-request (request)
  "Handle REQUEST sent from browser."
  (let* ((handler (cdr (assoc (plist-get request :name) sf--req-handlers-alist #'string=)))
         (request-id (or (plist-get request :id) "<unknown>"))
         (req-payload (mapcar
              (lambda (x)
                (pcase x
                  ;; because parsing JSON string to plist convert JS true/false
                  ;; to keywords :true/:false sometimes. Seems like a bug in Emacs's
                  ;; JSON parsing
                  (:false nil)
                  (:true t)
                  ("null" nil)
                  (val val)))
              (plist-get request :payload)))
         (res-payload (funcall handler req-payload)))
    (sf--send-message `(:requestId ,request-id :payload ,res-payload))))

(defun sf--register-req-handler (request handler)
  "Run HANDLER every time REQUEST is received from browser.
Return value of HANDLER is sent back to browser as response."
  (let ((cell (assoc request sf--req-handlers-alist #'string=)))
    (when cell (error "Handler already registered. There can only by one handler per request"))
    (push (cons request handler) sf--req-handlers-alist)))
;;; spookfox-request ends here

;;; spookfox-request-handlers
;;; Code for handling particular requests.
(defun sf--handle-toggle-tab-chaining (tab)
  "Handler for TOGGLE_TAB_CHAINING REQUEST."
  (let* ((chained? (not (plist-get tab :chained)))
         (tab-id (plist-get tab :id)))
    (if tab-id
        (sf--handle-update-tab (plist-put tab :chained chained?))
      (setq tab-id (sf--with-tabs-subtree
                    (goto-char (point-max))
                    (sf--insert-tab (plist-put tab :chained chained?)))))
    (sf--find-tab-with-id tab-id)))

(defun sf--handle-get-saved-tabs (_payload)
  "Handler for GET_SAVED_TABS."
  ;; Need to do the JSON encode/decode/encode dance again. I think we need a
  ;; different data structure to represent a Tab; plist is proving problematic
  ;; when we have to deal with list of Tabs
  (json-parse-string (concat "[" (string-join (mapcar #'json-encode (sf--get-saved-tabs)) ",") "]")))

(defun sf--handle-remove-tab (tab)
  "Handler for REMOVE_TAB action."
  (sf--remove-tab (plist-get tab :id)))

(defun sf--handle-update-tab (tab)
  "Handler for UPDATE_TAB action."
  (sf--update-tab
   (plist-get tab :id)
   (mapcar
    (lambda (x)
      (cond
       ((eq x nil) "nil")
       ((eq x t) "t")
       (t x))) tab)))

(sf--register-req-handler "TOGGLE_TAB_CHAINING" #'sf--handle-toggle-tab-chaining)
(sf--register-req-handler "GET_SAVED_TABS" #'sf--handle-get-saved-tabs)
(sf--register-req-handler "REMOVE_TAB" #'sf--handle-remove-tab)
(sf--register-req-handler "UPDATE_TAB" #'sf--handle-update-tab)
;;; spookfox-request-handlers ends here

;;; spookfox-native
(defvar spookfox-native-installation-dir (expand-file-name "spookfox" (concat user-emacs-directory "/.cache/"))
  "Location where spookfox-native will download the spookfox-binary.")

(defvar sf--native-installation-location (expand-file-name "spookfox-native" spookfox-native-installation-dir))

(defvar spookfox-version "0.1.8"
  "Spookfox version.")

(defun sf--download-native ()
  "Download spookfox-native into `spookfox-native-installation-dir'."
  (when (not (file-exists-p spookfox-native-installation-dir))
    (let* ((src-file "spookfox-native-x86_64-linux")
           (src (concat "https://github.com/bitspook/spookfox/releases/download/v"
                        spookfox-version "/" src-file)))
      (mkdir spookfox-native-installation-dir t)
      (url-copy-file src sf--native-installation-location)
      ;; (rename-file (expand-file-name src-file dest) sf--native-installation-location)
      (chmod sf--native-installation-location 365))))

(defun sf--native-manifest ()
  "Provide JSON manifest for spookfox-native."
  (json-encode `(:name "spookfox"
                 :description "Communicate between Emacs and Firefox"
                 :path ,sf--native-installation-location
                 :type "stdio"
                 :allowed_extensions ,(list "spookfox@bitspook.in"))))

(defun sf--native-manifest-location ()
  "Return location where spookfox-native manifest should be stored."
  (pcase system-type
    ('gnu/linux (expand-file-name "spookfox.json" (concat "~/.mozilla/native-messaging-hosts")))
    (_ (error "Unsupported system. Only gnu/linux is supported"))))

(defun sf--install-native ()
  "Download spookfox-native, and write spookfox-native manifest."
  (mkdir (file-name-directory (sf--native-manifest-location)) t)
  (sf--download-native)
  (with-temp-buffer
    (insert (sf--native-manifest))
    (write-file (sf--native-manifest-location))))
;;; spookfox-native ends here

;;; spookfox-interactive-functions
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
      (spookfox-request "OPEN_TAB" tab))
     ((string-match "https?:\/\/.*[\.].*" tab)
      (spookfox-request "OPEN_TAB" `(:url ,tab)))
     (t
      (spookfox-request "SEARCH_FOR" tab)))))

(defun spookfox-open-tab-group ()
  "Prompt user to select a tab group, and open all tabs in it."
  (interactive)
  (let* ((tabs (sf--get-saved-tabs))
         (groups (seq-uniq (seq-mapcat (lambda (tab) (plist-get tab :tags)) tabs)))
         (selected-group (completing-read "Select tab group: " groups nil t nil sf--tab-group-history))
         (group-tabs (seq-filter (lambda (tab) (seq-contains-p (plist-get tab :tags) selected-group #'string=)) tabs)))
    (spookfox-request
     "OPEN_TABS"
     (json-parse-string                 ; json-encode kinda messes up converting list
                                        ; of plists; so we make proper
                                        ; json-string, parses it to hashmap so
                                        ; spookfox-request can parse it again
                                        ; into a proper JSON array
      (concat "[" (string-join (mapcar #'json-encode group-tabs) ",") "]")))))

(defun spookfox-install-native ()
  "Download and install spookfox-native.
Interactively prompt to delete existing installation, if present."
  (interactive)
  (when (file-exists-p (sf--native-manifest-location))
    (when (yes-or-no-p "Existing spookfox-native manifest found. Delete it?")
      (delete-directory (file-name-directory (sf--native-manifest-location)) t)))

  (when (file-exists-p spookfox-native-installation-dir)
    (if (yes-or-no-p "Existing spookfox-native binary found. Delete it?")
        (delete-directory spookfox-native-installation-dir t)
      (message "Proceeding without re-downloading.")))

  (sf--install-native))
;;; spookfox-interactive-functions ends here

(spookfox-ensure-connection)

(provide 'spookfox)
;;; spookfox.el ends here
