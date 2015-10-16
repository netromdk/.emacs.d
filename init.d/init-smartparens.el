(require 'req-package)

(req-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (show-smartparens-global-mode 1)
    (smartparens-global-mode 1)))


(provide 'init-smartparens)
