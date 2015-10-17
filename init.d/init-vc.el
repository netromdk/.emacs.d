(require 'req-package)

(req-package magit
  :config
  (progn
    (setq magit-revert-item-confirm t)
    (setq magit-save-some-buffers t)
    (setq magit-auto-revert-mode nil) ;; Do _not_ auto-revert!
    (setq magit-revert-buffers t)
    (setq magit-last-seen-setup-instructions "1.4.0") ;; Silence latest info.
    (setq magit-push-always-verify nil) ;; Only ask when upstream is not conf'ed!

    (global-set-key (kbd "C-x g") 'magit-status)))

(req-package gitignore-mode)
(req-package gitconfig-mode)


(provide 'init-vc)
