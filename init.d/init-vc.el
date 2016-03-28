(require 'req-package)

(req-package magit
  :config
  (progn
    ;; Set defaults used by specific operations.
    (setq magit-pull-arguments '("--rebase"))
    (setq magit-cherry-pick-arguments '("-x"))
    (setq magit-log-arguments '("-n256" "--graph" "--decorate" "--color"))
    (setq magit-diff-arguments '("-U3" "--stat" "--no-ext-diff"))

    (global-set-key (kbd "C-x g") 'magit-status)))

;; Highlight uncommitted changes/additions/deletions in the fringe.
(req-package diff-hl
  :require magit
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  :config
  ;; Disabled flydiff mode because it somehow exceeds eval depth when used with magit...
  ;;(diff-hl-flydiff-mode 1)

  (global-diff-hl-mode 1))

(req-package gitignore-mode)
(req-package gitconfig-mode)

(req-package helm-ls-git
  :require helm
  :bind ("M-+" . helm-ls-git-ls))

;; Show git commit at line.
(req-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  (progn
    (setq git-messenger:show-detail t)))

(req-package git-timemachine
  :config
  (progn
    (defalias 'tm 'git-timemachine)))


(provide 'init-vc)
