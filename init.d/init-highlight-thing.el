(require 'req-package)

(req-package highlight-thing
  :config
  (progn
    (setq highlight-thing-delay-seconds 1.0)
    (global-highlight-thing-mode)))


(provide 'init-highlight-thing)
