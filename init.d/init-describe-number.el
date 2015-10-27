(require 'req-package)

(req-package describe-number
  :config
  (progn
    (global-set-key (kbd "M-?") 'describe-number-at-point)))


(provide 'init-describe-number)
