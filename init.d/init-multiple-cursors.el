;; Multiple cursors
(require 'req-package)

(req-package multiple-cursors
  :config
  (setq mc/mode-line
        (quote
         (" ["
          (:eval
           (format
            #("%d" 0 2
              (face font-lock-string-face))
            (mc/num-cursors)))
          " mc]")))
  (global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c a") 'mc/mark-all-like-this-dwim)
  (global-set-key (kbd "C-c m") 'mc/mark-all-dwim)
  (global-set-key (kbd "C-c l") 'mc/edit-lines))


(provide 'init-multiple-cursors)
