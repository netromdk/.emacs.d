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
  :require projectile helm helm-gtags hydra
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

  (defun msk/helm-update-gtags (arg)
    "Update gtags for all files or create if they don't already
exist. When given the prefix argument present gtags will be
removed and then recreated."
    (interactive "P")
    (let ((gtags-file (concat (projectile-project-root) "GTAGS"))
          (grtags-file (concat (projectile-project-root) "GRTAGS"))
          (gpath-file (concat (projectile-project-root) "GPATH")))
      (progn
        (when arg
          (message "Removing gtags..")
          (delete-file gtags-file)
          (delete-file grtags-file)
          (delete-file gpath-file))
        (if (file-exists-p gtags-file)
            (progn
              (message "Updating gtags..")
              (universal-argument)
              (helm-gtags-update-tags))
          (progn
            (message "Creating gtags..")
            (helm-gtags-create-tags (projectile-project-root) "default"))))))
  (msk/projectile-define-prefix-key "R" 'msk/helm-update-gtags)

  ;; Show hydra on "C-x p H".
  (defhydra hydra-projectile (:columns 4)
    "Projectile/Helm"
    ("f" helm-projectile-find-file "Find File")
    ("F" helm-projectile-find-file-dwim "Find File Dwim")
    ("o" helm-projectile-find-o "Find Other File")
    ("r" helm-projectile-recentf "Recent Files")
    ("d" helm-projectile-find-dir "Find Directory")

    ("a" helm-projectile-ag "Search with Ag")
    ("R" msk/helm-update-gtags "Update Gtags")

    ("s" helm-projectile-switch-project "Switch Project")
    ("c" projectile-invalidate-cache "Clear Cache")
    ("z" projectile-cache-current-file "Cache Current File")
    ("x" projectile-remove-known-project "Remove Known Project")
    ("X" projectile-cleanup-known-projects "Cleanup Known Projects")

    ("b" projectile-switch-to-buffer "Switch to Buffer")
    ("k" projectile-kill-buffers "Kill Buffers")

    ("q" nil "Cancel" :color blue))
  (msk/projectile-define-prefix-key "H" 'hydra-projectile/body))


(provide 'init-projectile)
