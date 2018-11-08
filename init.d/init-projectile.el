(require 'req-package)

(req-package projectile
  :init
  (defun msk/projectile-mode-line ()
    "Report project name in the mode line."
    (if (file-remote-p default-directory)
        "rρ"
      (if (string-equal "-" (projectile-project-name))
          "!ρ"
        "ρ")))
  (setq projectile-keymap-prefix (kbd "C-x p")
        projectile-enable-caching t
        projectile-mode-line-prefix "ρ"
        projectile-mode-line-function 'msk/projectile-mode-line)
  :config
  (projectile-global-mode))

(req-package helm-projectile
  :require (projectile helm helm-gtags helm-ag hydra magit)
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

  (defun msk/helm-do-ag-at-point ()
    "First select folder and then search using `helm-do-ag' with symbol at point, if anything."
    (interactive)
    (let ((helm-ag-insert-at-point 'symbol))
      (helm-do-ag)))

  (defun msk/helm-do-ag-buffers-at-point ()
    "Search open buffers using `helm-do-ag-buffers' with symbol at point, if anything."
    (interactive)
    (let ((helm-ag-insert-at-point 'symbol))
      (helm-do-ag-buffers)))

  (defun msk/helm-do-ag-this-file-at-point ()
    "Search current file using `helm-do-ag-this-file' with symbol at point, if anything."
    (interactive)
    (let ((helm-ag-insert-at-point 'symbol))
      (helm-do-ag-this-file)))

  (defun msk/helm-projectile-ag-at-point ()
    "Search using `helm-projectile-ag' (at project root) with symbol at point, if anything."
    (interactive)
    (let ((helm-ag-insert-at-point 'symbol))
      (helm-projectile-ag)))

  (defun msk/projectile-switch-project-magit (args)
    "Switch to project using projectile and run `magit-status'."
    (interactive "P")
    (let ((projectile-switch-project-action 'magit-status))
      (helm-projectile-switch-project)))

  (defhydra projectile-hydra (:idle 1 :hint nil)
    "
Projectile: %(projectile-project-root)

     Find                Search                Buffers                Cache/Project
-------------------------------------------------------------------------------------------
  _f_: File            _ss_: Ag (at point)      _b_: Switch to buffer    _p_: Switch project (find file)
  _F_: File dwim       _sb_: Ag (buffers)       _k_: Kill all buffers    _m_: Switch project (magit)
  _o_: Other file      _sp_: Ag (project root)                         ^^_c_: Cache clear
  _r_: Recent file     _sf_: Ag (this file)                            ^^_x_: Remove known project
  _d_: Dir                                                           ^^^^_X_: Cleanup non-existing
  _w_: File other win                                                ^^^^_z_: Cache current file

"
    ("f" helm-projectile-find-file)
    ("F" helm-projectile-find-file-dwim)
    ("o" helm-projectile-find-other-file)
    ("r" helm-projectile-recentf)
    ("d" helm-projectile-find-dir)
    ("w" projectile-find-file-other-window)

    ("ss" msk/helm-do-ag-at-point)
    ("sb" msk/helm-do-ag-buffers-at-point)
    ("sp" msk/helm-projectile-ag-at-point) ;; at project root
    ("sf" msk/helm-do-ag-this-file-at-point)
    ;;("g" msk/helm-update-gtags)
    ;;("O" projectile-multi-occur :color blue)

    ("b" helm-projectile-switch-to-buffer)
    ("k" projectile-kill-buffers)

    ("p" helm-projectile-switch-project)
    ("m" msk/projectile-switch-project-magit :color blue)
    ("c" projectile-invalidate-cache)
    ("z" projectile-cache-current-file)
    ("x" projectile-remove-known-project)
    ("X" projectile-cleanup-known-projects)

    ("M" magit-status "Magit" :color blue)
    ("q" nil "Cancel" :color blue))

  (define-key projectile-mode-map projectile-keymap-prefix 'projectile-hydra/body))


(provide 'init-projectile)
