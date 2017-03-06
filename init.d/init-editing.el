(require 'req-package)

;; Multiple cursors
(req-package multiple-cursors
  :config
  (global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c m") 'mc/mark-all-dwim))

;; Convenient word transformations.
(req-package fix-word
  :config
  (global-set-key (kbd "M-u") 'fix-word-upcase)
  (global-set-key (kbd "M-l") 'fix-word-downcase)
  (global-set-key (kbd "M-c") 'fix-word-capitalize))


(provide 'init-editing)
