(require 'req-package)

(req-package dired
  :require (ls-lisp key-chord)
  :config
  (setq dired-listing-switches "-lha") ;; Show human-readable sizes.

  ;; In dired mode, use Emacs's emulation of "ls" because the system one most
  ;; often doesn't support the "--dired" argument.
  (setq ls-lisp-use-insert-directory-program nil)

  ;; Automatically refresh dired buffer on changes.
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  (key-chord-define dired-mode-map "qq" 'dired-up-directory))

;; dired-k makes directory listings more readable, adding a bit of color and some git status
;; information on files and directories.
(req-package dired-k
  :require dired
  :config
  ;; Run dired-k when dired buffer is opened.
  (add-hook 'dired-initial-position-hook 'dired-k)

  (define-key dired-mode-map "K" 'dired-k))


(provide 'init-dired)
