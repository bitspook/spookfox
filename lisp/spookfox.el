;;; package --- Communicate with a browser which have spookfox browser addon installed. -*- lexical-binding: t -*-
;;;
;; Copyright © 2022 bitspook
;;
;; Author: bitspook
;; Homepage: https://github.com/bitspook/spookfox
;; Keywords: Firefox
;; Version: 0.2.0
;; Package-Requires: ((websocket "1.13"))
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

(require 'spookfox-core)
(require 'spookfox-org-tabs)

(setf spookfox-available-apps `((org-tabs . ,#'spookfox--org-tabs-init)))
(setf spookfox-enabled-apps '(org-tabs))

(defun spookfox-init ()
  "Initialize spookfox with enabled apps."
  (dolist (app spookfox-enabled-apps)
    (spookfox--enable-app app))

  (spookfox-start-server))

(provide 'spookfox)
;;; spookfox.el ends here
