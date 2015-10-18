(require 'req-package)

(req-package ls-lisp
  :config
  (progn
    ;; In dired mode, use Emacs's emulation of "ls" because the system one most
    ;; often doesn't support the "--dired" argument.
    (setq ls-lisp-use-insert-directory-program nil)))


(provide 'init-dired)
