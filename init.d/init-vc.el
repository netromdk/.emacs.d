(require 'req-package)

(req-package magit
  :config
  ;; Set defaults used by specific operations.
  (setq magit-pull-arguments '("--rebase")
        magit-cherry-pick-arguments '("-x")
        magit-log-arguments '("-n50" "--graph" "--decorate" "--color")
        magit-diff-arguments '("-U3" "--stat" "--no-ext-diff")
        magit-fetch-arguments '("--prune"))

  ;; Show status full screen.
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  ;; Bindings.
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; Protect against accident pushes to upstream/push remote.
  (defadvice magit-push-current-to-upstream
      (around my-protect-accidental-magit-push-current-to-upstream)
    "Protect against accidental push to upstream.

Causes `magit-git-push' to ask the user for confirmation first."
    (let ((my-magit-ask-before-push t))
      ad-do-it))

  (defadvice magit-push-current-to-pushremote
      (around my-protect-accidental-magit-push-current-to-pushremote)
    "Protect against accidental push to push remote.

Causes `magit-git-push' to ask the user for confirmation first."
    (let ((my-magit-ask-before-push t))
      ad-do-it))

  (defadvice magit-git-push (around my-protect-accidental-magit-git-push)
    "Maybe ask the user for confirmation before pushing.

Advice to `magit-push-current-to-upstream' triggers this query."
    (if (bound-and-true-p my-magit-ask-before-push)
        ;; Arglist is (BRANCH TARGET ARGS)
        (if (yes-or-no-p (format "Push %s branch upstream to %s? "
                                 (ad-get-arg 0) (ad-get-arg 1)))
            ad-do-it
          (error "Push to upstream aborted by user"))
      ad-do-it))

  (ad-activate 'magit-push-current-to-upstream)
  (ad-activate 'magit-push-current-to-pushremote)
  (ad-activate 'magit-git-push))

;; Highlight uncommitted changes/additions/deletions in the fringe.
(req-package diff-hl
  :require magit
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  (diff-hl-flydiff-mode 1)
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
  (setq git-messenger:show-detail t))

(req-package git-timemachine
  :config
  (defalias 'tm 'git-timemachine))


(provide 'init-vc)
