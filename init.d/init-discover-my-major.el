(require 'req-package)

(req-package discover-my-major
  :config
  (global-set-key (kbd "C-h C-m") 'discover-my-major)
  (global-set-key (kbd "C-h M-m") 'discover-my-mode))


(provide 'init-discover-my-major)
