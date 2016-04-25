(require 'req-package)

(req-package savehist
  :require helm
  :config
  ;; Saves mini buffer history including search and kill ring values, compile history, and helm
  ;; find-files history.
  (setq savehist-additional-variables
        '(search-ring
          regexp-search-ring
          query-replace-history
          kill-ring
          compile-history
          helm-ff-history))
  (setq savehist-autosave-interval 60)
  (setq savehist-file (concat user-emacs-directory "savehist"))
  (savehist-mode t))

(req-package recentf
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-save-file (concat user-emacs-directory "recentf")
        recentf-auto-cleanup 300        ; Cleanup every 5 minutes of idle time.
        recentf-exclude '("ido\\.last"
                          "\\.emacs\\.d/saveplace"
                          "\\.emacs\\.d/savehist"
                          "\\.emacs\\.d/recentf"
                          "\\.git/COMMIT_EDITMSG"
                          ".*-autoloads\\.el"
                          "/elpa/.*"))
  (recentf-mode 1)
  (global-set-key "\C-xr" 'recentf-open-files))

;; Saves cursor positions of visited files.
(req-package saveplace
  :config
  (setq save-place-limit 400)
  (setq save-place-file (concat user-emacs-directory "saveplace"))
  (setq-default save-place t))


(provide 'init-session)
