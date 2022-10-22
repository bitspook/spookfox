;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((typescript-mode . ((eval .
                           (add-to-list 'exec-path (concat (locate-dominating-file default-directory ".dir-locals.el") "node_modules/.bin/"))))))
