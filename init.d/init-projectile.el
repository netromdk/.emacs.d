(require 'req-package)

(req-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-x p")
        projectile-enable-caching t
        projectile-mode-line
        '(:eval
          (if (file-remote-p default-directory)
              "rρ"
            (if (string-equal "-" (projectile-project-name))
                "!ρ"
              "ρ"))))
  :config
  (projectile-global-mode))

(req-package helm-projectile
  :require (projectile helm helm-gtags hydra magit treemacs)
  :config
  (setq helm-projectile-fuzzy-match t
        projectile-switch-project-action 'helm-projectile-find-file)

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

  (defun msk/projectile-switch-project-treemacs (args)
    "Switch to project using projectile and run `treemacs-find-file'."
    (interactive "P")
    (helm-projectile-switch-project)
    (treemacs-find-file))

  (defun msk/projectile-switch-project-magit (args)
    "Switch to project using projectile and run `magit-status'."
    (interactive "P")
    (let ((projectile-switch-project-action 'magit-status))
      (helm-projectile-switch-project)))

  (defhydra projectile-hydra (:idle 1 :hint nil)
    "
Projectile: %(projectile-project-root)

     Find               Search/Tags          Buffers                Cache/Project
------------------------------------------------------------------------------------------
  _f_: File            _a_: Ag                _b_: Switch to buffer    _p_: Switch project (find file)
  _F_: File dwim       _g_: Update gtags      _k_: Kill all buffers    _m_: Switch project (magit)
  _o_: Other file      _O_: Multi-occur                              ^^_c_: Cache clear
  _r_: Recent file                                                 ^^^^_x_: Remove known project
  _d_: Dir                                                         ^^^^_X_: Cleanup non-existing
  _w_: File other win                                              ^^^^_z_: Cache current file

"
    ("f" helm-projectile-find-file)
    ("F" helm-projectile-find-file-dwim)
    ("o" helm-projectile-find-other-file)
    ("r" helm-projectile-recentf)
    ("d" helm-projectile-find-dir)
    ("w" projectile-find-file-other-window)

    ("a" helm-projectile-ag)
    ("g" msk/helm-update-gtags)
    ("O" projectile-multi-occur :color blue)

    ("b" helm-projectile-switch-to-buffer)
    ("k" projectile-kill-buffers)

    ("p" msk/projectile-switch-project-treemacs)
    ("m" msk/projectile-switch-project-magit :color blue)
    ("c" projectile-invalidate-cache)
    ("z" projectile-cache-current-file)
    ("x" projectile-remove-known-project)
    ("X" projectile-cleanup-known-projects)

    ("M" magit-status "Magit" :color blue)
    ("q" nil "Cancel" :color blue))

  (define-key projectile-mode-map projectile-keymap-prefix 'projectile-hydra/body))


(provide 'init-projectile)
