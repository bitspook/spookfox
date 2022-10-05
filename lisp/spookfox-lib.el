;;; package ---  Utility functions for spookfox -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defun spookfox--string-blank-p (str)
  "Return t if STR is blank.
Considers hard-space (ASCII 160) as space."
  (string-blank-p (string-replace (concat '(160)) "" str)))

(provide 'spookfox-lib)
;;; spookfox-lib.el ends here
