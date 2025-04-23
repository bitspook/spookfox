(use-modules (guix packages)
             (gnu packages node))

(packages->manifest (list node))
