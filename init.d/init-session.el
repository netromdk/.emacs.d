(require 'req-package)

;; Saves mini buffer history including search and kill ring values.
(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))
(setq savehist-autosave-interval 60)
(setq savehist-file (concat user-emacs-directory "savehist"))
(savehist-mode t)

(req-package recentf
  :config
  (progn
    (setq recentf-max-saved-items 200)
    (setq recentf-max-menu-items 15)
    (setq recentf-save-file (concat user-emacs-directory "recentf"))
    (recentf-mode 1)
    (global-set-key "\C-xr" 'recentf-open-files)))

;; Saves cursor positions of visited files.
(req-package saveplace
  :config
  (progn
    (setq save-place-file (concat user-emacs-directory "saveplace"))
    (setq-default save-place t)))


(provide 'init-session)
