(require 'req-package)

(req-package describe-number
  :config
  (global-set-key (kbd "M-?") 'describe-number-at-point))


(provide 'init-describe-number)
