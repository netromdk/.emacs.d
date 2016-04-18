(require 'req-package)

(req-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))
  (setq projectile-enable-caching t)
  (setq projectile-mode-line
        '(:eval
          (if (file-remote-p default-directory)
              "rρ"
            (if (string-equal "-" (projectile-project-name))
                "!ρ"
              "ρ"))))
  :config
  (projectile-global-mode))

(req-package helm-projectile
  :require projectile helm helm-gtags
  :config
  (setq helm-projectile-fuzzy-match t)
  (setq projectile-switch-project-action 'helm-projectile-find-file)

  ;; Use helm-projectile alternatives.
  (defun msk/projectile-define-prefix-key (key func)
    (define-key projectile-mode-map
      (kbd (concat projectile-keymap-prefix key)) func))
  (msk/projectile-define-prefix-key "f" 'helm-projectile-find-file)
  (msk/projectile-define-prefix-key "d" 'helm-projectile-find-dir)
  (msk/projectile-define-prefix-key "o" 'helm-projectile-find-other-file)
  (msk/projectile-define-prefix-key "a" 'helm-projectile-ag)
  (msk/projectile-define-prefix-key "p" 'helm-projectile-switch-project)
  (msk/projectile-define-prefix-key "b" 'helm-projectile-switch-to-buffer)
  (msk/projectile-define-prefix-key "r" 'helm-projectile-recentf)

  ;; Update gtags for all files or create if it doesn't already exist.
  (defun msk/helm-update-gtags ()
    (interactive)
    (if (file-exists-p (concat (projectile-project-root) "GTAGS"))
        (progn
          (universal-argument)
          (helm-gtags-update-tags))
      (helm-gtags-create-tags (projectile-project-root) "default")))
  (msk/projectile-define-prefix-key "R" 'msk/helm-update-gtags))


(provide 'init-projectile)
