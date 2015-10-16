(require 'req-package)

(req-package recentf
  :config
  (progn
    (recentf-mode 1)
    (global-set-key "\C-xr" 'recentf-open-files)))


(provide 'init-recent)
