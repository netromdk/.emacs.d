(require 'req-package)

(req-package js
  :config
  (progn
    (add-hook 'js-mode-hook
              (lambda () (setq js-indent-level 2)))))


(provide 'init-js)
