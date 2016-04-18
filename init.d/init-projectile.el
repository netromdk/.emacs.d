(require 'req-package)

(req-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))
  (setq projectile-enable-caching t)

  ;; If remote folder then show "rρ", if not a projectile project then "!ρ", otherwise simply "ρ".
  (setq projectile-mode-line '(:eval
                               (if (file-remote-p default-directory)
                                   "rρ"
                                 (if (string-equal "-" (projectile-project-name))
                                     "!ρ"
                                   "ρ"))))
  :config
  (projectile-global-mode))

(req-package helm-projectile
  :require projectile helm
  :config
  (setq helm-projectile-fuzzy-match t)
  (setq projectile-switch-project-action 'helm-projectile-find-file)

  ;; Use helm-projectile alternatives.
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "f")) 'helm-projectile-find-file)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "d")) 'helm-projectile-find-dir)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "o")) 'helm-projectile-find-other-file)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "a")) 'helm-projectile-ag)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "p")) 'helm-projectile-switch-project)
  (define-key projectile-mode-map
    (kbd (concat projectile-keymap-prefix "b")) 'helm-projectile-switch-to-buffer))


(provide 'init-projectile)
