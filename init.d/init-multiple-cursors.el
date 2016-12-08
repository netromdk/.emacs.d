;; Multiple cursors
(require 'req-package)

(req-package multiple-cursors
  :config
  (global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c m") 'mc/mark-all-dwim))


(provide 'init-multiple-cursors)
