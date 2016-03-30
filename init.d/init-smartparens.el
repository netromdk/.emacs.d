(require 'req-package)

;; Use smartparens only to highlight matches of parens/blocks. No extra
;; insertion because it's annoying!
(req-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1))


(provide 'init-smartparens)
