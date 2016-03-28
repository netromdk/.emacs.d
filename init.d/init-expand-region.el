(require 'req-package)

(req-package expand-region
  :config
  (progn
    (global-set-key (kbd "M-m") 'er/expand-region)))


(provide 'init-expand-region)
