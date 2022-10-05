;;; package -- Spookfox app to manage browser tabs as org-mode subtree

;;; Commentary:
;; Access, save and manipulate browser tabs

;;; Code:
(defvar spookfox-saved-tabs-target `(file+headline ,(expand-file-name "spookfox.org" org-directory) "Tabs")
  "Target parse-able by org-capture-template where browser tabs will be saved.")

(defvar sf--tab-history nil
  "History of accessing spookfox tabs.")

(defvar sf--tab-group-history nil
  "History of accessing spookfox tab groups.")

(defvar sf--known-tab-props '("id" "url" "chained" "id")
  "List of properties which are read when an org node is converted to a tab.")

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
         ((string= "TITLE" prop) (org-edit-headline (if (sf--string-blank-p val) "<empty-title>" val)))
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
             ("TITLE" (org-edit-headline (if (sf--string-blank-p val) "<empty-title>" val)))
             ("TAGS" (org-set-tags val))
             (_ (org-entry-put (point) prop val)))))
       (save-buffer))))
  (sf--find-tab-with-id tab-id))

(defun sf--remove-tab (tab-id)
  "Remove tab with TAB-ID."
  (let ((tab (sf--find-tab-with-id tab-id)))
    (sf--with-tabs-subtree
     (let ((pos (org-id-find-id-in-file tab-id (buffer-file-name))))
       (when pos
         (goto-char (cdr pos))
         (org-narrow-to-subtree)
         (delete-region (point-min) (point-max))
         (widen)
         (delete-line)
         (save-buffer))))
    tab))

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

(defun sf--handle-get-saved-tabs (_payload)
  "Handler for GET_SAVED_TABS."
  ;; Need to do the JSON encode/decode/encode dance again. I think we need a
  ;; different data structure to represent a Tab; plist is proving problematic
  ;; when we have to deal with list of Tabs
  (json-parse-string (concat "[" (string-join (mapcar #'json-encode (sf--get-saved-tabs)) ",") "]")))

(defun sf--handle-remove-tab (tab)
  "Handler for REMOVE_TAB action."
  (sf--remove-tab (plist-get tab :id)))

(defun sf--handle-update-tab (payload)
  "Handler for UPDATE_TAB action.
PAYLOAD is a plist with :id and :patch"
  (sf--update-tab
   (plist-get payload :id)
   (mapcar
    (lambda (x)
      (cond
       ((eq x nil) "nil")
       ((eq x t) "t")
       (t x))) (plist-get payload :patch))))

(defun sf--handle-toggle-tab-chaining (tab)
  "Handler for TOGGLE_TAB_CHAINING REQUEST."
  (let* ((chained? (not (plist-get tab :chained)))
         (tab-id (plist-get tab :id)))
    (if tab-id
        (sf--handle-update-tab `(:id ,tab-id :patch (:chained ,chained?)))
      (setq tab-id (sf--with-tabs-subtree
                    (goto-char (point-max))
                    (sf--insert-tab (plist-put tab :chained chained?)))))
    (sf--find-tab-with-id tab-id)))

(sf--register-req-handler "TOGGLE_TAB_CHAINING" #'sf--handle-toggle-tab-chaining)
(sf--register-req-handler "GET_SAVED_TABS" #'sf--handle-get-saved-tabs)
(sf--register-req-handler "REMOVE_TAB" #'sf--handle-remove-tab)
(sf--register-req-handler "UPDATE_TAB" #'sf--handle-update-tab)

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
  "Prompt for a tab group, and open all tabs belonging to that group."
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
;;; spookfox-tabs ends here

(provide 'spookfox-org-tabs)
;;; spookfox-org-tabs.el ends here
