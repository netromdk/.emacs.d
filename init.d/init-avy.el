(require 'req-package)

(req-package avy
  :config
  (progn
    (global-set-key (kbd "C-c 1") 'avy-goto-char)
    (global-set-key (kbd "C-c 2") 'avy-goto-char-2)))


(provide 'init-avy)
