(use-modules (guix packages)
             (gnu packages emacs))

(packages->manifest (list emacs))
