(require 'req-package)

(req-package ls-lisp
  :init
  ;; Automatically refresh dired buffer on changes.
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  :config
  (progn
    (setq dired-listing-switches "-lha") ;; Show human-readable sizes.

    ;; In dired mode, use Emacs's emulation of "ls" because the system one most
    ;; often doesn't support the "--dired" argument.
    (setq ls-lisp-use-insert-directory-program nil)))


(provide 'init-dired)
