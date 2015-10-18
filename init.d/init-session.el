(require 'req-package)

;; Saves mini buffer history.
(savehist-mode t)

(req-package recentf
  :config
  (progn
    (recentf-mode 1)
    (global-set-key "\C-xr" 'recentf-open-files)))

;; Saves cursor positions of visited files.
(req-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/saveplace.txt")))


(provide 'init-session)
