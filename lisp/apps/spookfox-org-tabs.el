;;; spookfox-org-tabs -- Spookfox app to manage browser tabs as org-mode subtree -*- lexical-binding: t -*-

;;; Commentary:
;; ABANDONED. Please DO NOT USE THIS. I am keeping it around until I can change it to "archive tabs
;; to org file".
;;
;; Access, save and manipulate browser tabs

;;; Code:
(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'spookfox)
(require 'spookfox-tabs)

(defvar spookfox-saved-tabs-target `(file+headline ,(expand-file-name "spookfox.org" user-emacs-directory) "Tabs")
  "Target parse-able by org-capture-template where browser tabs will be saved.")

(defvar spookfox--tab-history nil
  "History of accessing spookfox tabs.")

(defvar spookfox--tab-group-history nil
  "History of accessing spookfox tab groups.")

(defvar spookfox--known-tab-props '("id" "url" "chained" "id")
  "List of properties which are read when an org node is converted to a tab.")

(defvar spookfox-org-tabs--msg-prefix "OT_")

(defun spookfox-org-tabs--request (&rest args)
  "Make spookfox-request with CLIENT and ARGS but with prefixed NAME."
  (let ((spookfox--msg-prefix spookfox-org-tabs--msg-prefix))
    (apply #'spookfox-request args)))

(defun spookfox-org-tabs--request-active-tab ()
  "Get details of active tab in browser."
  (let ((client (cl-first spookfox--connected-clients)))
    (when client
      (plist-get
       (spookfox--poll-response (spookfox-org-tabs--request client "GET_ACTIVE_TAB"))
       :payload))))

(defun spookfox-org-tabs--request-all-tabs ()
  "Get all tabs currently present in browser."
  (spookfox--request-all-tabs))

(defun spookfox-org-tabs--insert-tab (tab)
  "Insert browser TAB as a new org-mode-subtree."
  (org-insert-heading)
  (let ((id (org-id-get-create)))
    (while tab
      (let ((prop (upcase (substring (format "%s" (pop tab)) 1)))
            (val (pop tab)))
        (cond
         ((string= "TITLE" prop) (org-edit-headline (if (spookfox--string-blank-p val) "<empty-title>" val)))
         ((string= "TAGS" prop) (org-set-tags val))
         (t (org-entry-put (point) prop (format "%s" val))))))
    id))

(defun spookfox-org-tabs--deserialize-tab ()
  "Return spookfox tab for subtree at point.
This function is useful for `org-map-entries`."
  (let ((props (org-entry-properties)))
    `(:title ,(alist-get "ITEM" props nil nil #'string=)
      :tags ,(mapcar #'substring-no-properties (org-get-tags))
      ,@(seq-reduce (lambda (accum cell)
                      (when (seq-contains-p spookfox--known-tab-props (downcase (car cell)) #'string=) ;; org-mode upcase all the property names
                        (setq accum (plist-put accum
                                               (intern (downcase (format ":%s" (car cell))))
                                               (pcase (cdr cell)
                                                 ("t" t)
                                                 ("nil" nil)
                                                 (val val)))))
                      accum)
                    props nil))))

(defun spookfox-org-tabs--save-tabs (tabs &optional hide-prompt?)
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
    ;; start with a valid org-entry, but `spookfox-org-tabs--insert-tab' adds its own entries
    ;; later.
    (delete-char -3)
    (let ((start-pos (point)))
      (seq-map (lambda (tab)
                 (spookfox-org-tabs--insert-tab tab)
                 (goto-char (point-max))) tabs)
      (goto-char start-pos))
    (recenter-top-bottom)))

(defmacro spookfox-org-tabs--with-tabs-subtree (&rest body)
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

(defun spookfox-org-tabs--get-saved-tabs ()
  "Get browser tabs saved with spookfox.
Returns a list of tabs as plists. Any subtree which don't have a
ID and URL is discarded."
  (seq-filter
   #'spookfox-org-tabs--tab-p
   (spookfox-org-tabs--with-tabs-subtree
    (org-map-entries #'spookfox-org-tabs--deserialize-tab))))

(defun spookfox-org-tabs--find-tab-with-id (tab-id)
  "Find tab with TAB-ID."
  (spookfox-org-tabs--with-tabs-subtree
   (let ((pos (org-id-find-id-in-file tab-id (buffer-file-name))))
     (when pos
       (goto-char (cdr pos))
       (spookfox-org-tabs--deserialize-tab)))))

(defun spookfox-org-tabs--update-tab (tab-id patch)
  "Update a saved tab matching TAB-ID with PATCH.
PATCH is a plist of properties to upsert."
  (spookfox-org-tabs--with-tabs-subtree
   (let ((pos (org-id-find-id-in-file tab-id (buffer-file-name))))
     (when pos
       (goto-char (cdr pos))
       (while patch
         (let ((prop (upcase (substring (symbol-name (pop patch)) 1)))
               (val (pop patch)))
           (pcase prop
             ;; Empty titles have been observed in the wild
             ("TITLE" (org-edit-headline (if (spookfox--string-blank-p val) "<empty-title>" val)))
             ("TAGS" (org-set-tags val))
             (_ (org-entry-put (point) prop val)))))
       (save-buffer))))
  (spookfox-org-tabs--find-tab-with-id tab-id))

(defun spookfox-org-tabs--remove-tab (tab-id)
  "Remove tab with TAB-ID."
  (let ((tab (spookfox-org-tabs--find-tab-with-id tab-id)))
    (spookfox-org-tabs--with-tabs-subtree
     (let ((pos (org-id-find-id-in-file tab-id (buffer-file-name))))
       (when pos
         (goto-char (cdr pos))
         (org-narrow-to-subtree)
         (delete-region (point-min) (point-max))
         (widen)
         (delete-line)
         (save-buffer))))
    tab))

(defun spookfox-org-tabs--tab-read ()
  "Ask user to select a tab using Emacs' completion system."
  (let* ((tabs (mapcar
                (lambda (pl)
                  (cons
                   (concat (plist-get pl :title) "\t\t(" (plist-get pl :url) ")" "[" (plist-get pl :id) "]")
                   pl))
                (spookfox-org-tabs--get-saved-tabs)))
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
               nil nil nil 'spookfox--tab-history)))
    (or (cdr (assoc tab tabs))
        tab)))

(defun spookfox-org-tabs--tab-p (tab)
  "Return t if TAB is a spookfox tab, nil otherwise."
  (when (and (plist-get tab :id) (plist-get tab :url)) t))

(defun spookfox-org-tabs--handle-get-saved-tabs (_payload)
  "Handler for GET_SAVED_TABS."
  ;; Need to do the JSON encode/decode/encode dance again. I think we need a
  ;; different data structure to represent a Tab; plist is proving problematic
  ;; when we have to deal with list of Tabs
  (json-parse-string (concat "[" (string-join (mapcar #'json-encode (spookfox-org-tabs--get-saved-tabs)) ",") "]")))

(defun spookfox-org-tabs--handle-remove-tab (tab)
  "Handler for REMOVE_TAB action."
  (spookfox-org-tabs--remove-tab (plist-get tab :id)))

(defun spookfox-org-tabs--handle-update-tab (payload)
  "Handler for UPDATE_TAB action.
PAYLOAD is a plist with :id and :patch"
  (spookfox-org-tabs--update-tab
   (plist-get payload :id)
   (mapcar
    (lambda (x)
      (cond
       ((eq x nil) "nil")
       ((eq x t) "t")
       (t x))) (plist-get payload :patch))))

(defun spookfox-org-tabs--handle-toggle-tab-chaining (tab)
  "Handler for TOGGLE_TAB_CHAINING REQUEST."
  (let* ((chained? (not (plist-get tab :chained)))
         (tab-id (plist-get tab :id)))
    (if tab-id
        (spookfox-org-tabs--handle-update-tab `(:id ,tab-id :patch (:chained ,chained?)))
      (setq tab-id (spookfox-org-tabs--with-tabs-subtree
                    (goto-char (point-max))
                    (spookfox-org-tabs--insert-tab (plist-put tab :chained chained?)))))
    (spookfox-org-tabs--find-tab-with-id tab-id)))

(defun spookfox-org-tabs-save-all-tabs ()
  "Save all currently open browser tabs at `spookfox-saved-tabs-target`.
It will open a capture buffer so user get a chance to preview and
make changes."
  (interactive)
  (let ((tabs (spookfox-org-tabs--request-all-tabs)))
    (spookfox-org-tabs--save-tabs tabs)))

(defun spookfox-org-tabs-save-active-tab ()
  "Save active tab in browser."
  (interactive)
  (let ((tab (spookfox-org-tabs--request-active-tab)))
    (spookfox-org-tabs--save-tabs (list tab))))

(defun spookfox-org-tabs-open ()
  "Prompt user to select a tab and open it in spookfox browser."
  (interactive)
  (let ((tab (spookfox-org-tabs--tab-read))
        (client (cl-first spookfox--connected-clients)))
    (when client
      (cond
       ((spookfox-org-tabs--tab-p tab)
        (spookfox-org-tabs--request client "OPEN_TAB" tab))
       ((string-match "https?:\/\/.*[\.].*" tab)
        (spookfox-org-tabs--request client "OPEN_TAB" `(:url ,tab)))
       (t
        (spookfox-org-tabs--request client "SEARCH_FOR" tab))))))

(defun spookfox-org-tabs-open-group ()
  "Prompt for a tab group, and open all tabs belonging to that group."
  (interactive)
  (let* ((tabs (spookfox-org-tabs--get-saved-tabs))
         (groups (seq-uniq (seq-mapcat (lambda (tab) (plist-get tab :tags)) tabs)))
         (selected-group (completing-read "Select tab group: " groups nil t nil spookfox--tab-group-history))
         (group-tabs (seq-filter (lambda (tab) (seq-contains-p (plist-get tab :tags) selected-group #'string=)) tabs))
         (client (cl-first spookfox--connected-clients)))
    (when client
      (spookfox-org-tabs--request
       client
       "OPEN_TABS"
       (json-parse-string        ; json-encode kinda messes up converting list
                                        ; of plists; so we make proper
                                        ; json-string, parses it to hashmap so
                                        ; spookfox-org-tabs--request can parse it again
                                        ; into a proper JSON array
        (concat "[" (string-join (mapcar #'json-encode group-tabs) ",") "]"))))))

;; This should be added to spookfox-client-connected-hook
(defun spookfox-org-tabs--on-init ()
  "Initialize spookfox-org-tabs app."
  (let ((spookfox--msg-prefix spookfox-org-tabs--msg-prefix))
    (spookfox--register-req-handler "TOGGLE_TAB_CHAINING" #'spookfox-org-tabs--handle-toggle-tab-chaining)
    (spookfox--register-req-handler "GET_SAVED_TABS" #'spookfox-org-tabs--handle-get-saved-tabs)
    (spookfox--register-req-handler "REMOVE_TAB" #'spookfox-org-tabs--handle-remove-tab)
    (spookfox--register-req-handler "UPDATE_TAB" #'spookfox-org-tabs--handle-update-tab)))

(provide 'spookfox-org-tabs)
;;; spookfox-org-tabs.el ends here
