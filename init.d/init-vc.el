(require 'req-package)

(req-package magit
  :config
  ;; Set defaults used by specific operations.
  (setq magit-pull-arguments '("--rebase")
        magit-cherry-pick-arguments '("-x")
        magit-log-arguments '("-n256" "--graph" "--decorate" "--color")
        magit-diff-arguments '("-U3" "--stat" "--no-ext-diff"))

  ;; Show status full screen.
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (global-set-key (kbd "C-x g") 'magit-status))

;; Highlight uncommitted changes/additions/deletions in the fringe.
(req-package diff-hl
  :require magit
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  ;; Disabled flydiff mode because it somehow exceeds eval depth when used with magit...
  ;;(diff-hl-flydiff-mode 1)

  (global-diff-hl-mode 1))

(req-package gitignore-mode)
(req-package gitconfig-mode)

(req-package git-commit
  :config
  (add-hook 'git-commit-mode-hook
            '(lambda ()
               (setq git-commit-fill-column global-fill-column)
               ;; Make sure it's run as the absolute last. Something else was turning it on anyways.
               (run-at-time "1 sec" nil
                            (lambda ()
                              (auto-fill-mode 0)
                              (visual-line-mode t))))))

(req-package helm-ls-git
  :require helm
  :bind ("M-+" . helm-ls-git-ls))

;; Show git commit at line.
(req-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t))

(req-package git-timemachine
  :config
  (defalias 'tm 'git-timemachine))


(provide 'init-vc)
