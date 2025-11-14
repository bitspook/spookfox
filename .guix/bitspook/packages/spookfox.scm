(define-module (bitspook packages spookfox)
  #:use-module (guix)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages node)
  #:use-module (gnu packages base)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (guix licenses)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz))

(define-public emacs-spookfox
  (package
    (name "emacs-spookfox")
    (version "0.8.0")
    (source (local-file "../../../lisp" #:recursive? #t))
    (build-system emacs-build-system)
    (native-inputs (list gnu-make node emacs-minimal guile-next guile-ares-rs))
    (propagated-inputs (list emacs-websocket))
    (synopsis "A tinkerer's bridge between Emacs and Firefox")
    (description "A tinkerer's bridge between Emacs and Firefox")
    (home-page "https://bitspook.in/projects/spookfox")
    (license gpl3+)))

emacs-spookfox
