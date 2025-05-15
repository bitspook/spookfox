(use-modules (guix packages)
             (gnu packages node)
             (gnu packages base))

(packages->manifest (list gnu-make node))
