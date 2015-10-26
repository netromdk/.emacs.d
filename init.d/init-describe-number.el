(require 'req-package)

(req-package describe-number
  :config
  (progn
    (global-set-key (kbd "C-c ?") 'describe-number-at-point)))


(provide 'init-describe-number)
