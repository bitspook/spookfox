;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((typescript-mode . ((eval .
                           (setf
                            flycheck-javascript-eslint-executable
                            (expand-file-name "spookfox-addon/node_modules/.bin/eslint" (projectile-project-root)))))))
