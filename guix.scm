(use-modules
 (bitspook packages spookfox)
 (guix)
 (guix packages)
 (guix git-download)
 (gnu packages node)
 (gnu packages base)
 (guix build-system emacs)
 (gnu packages guile)
 (gnu packages guile-xyz)
 (gnu packages emacs)
 (gnu packages emacs-xyz))

(package
  (inherit emacs-spookfox)
  (version "0.8.1-dev")
  (source (local-file
           "./" "sf-checkout"
           #:recursive? #t
           #:select? (git-predicate (current-source-directory))))
  (native-inputs (list gnu-make node emacs-minimal guile-next guile-ares-rs))
  (propagated-inputs (list emacs-websocket)))
