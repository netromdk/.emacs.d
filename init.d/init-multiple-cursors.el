;; Multiple cursors
(require 'req-package)

(req-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c m") 'mc/mark-all-like-this)))


(provide 'init-multiple-cursors)
