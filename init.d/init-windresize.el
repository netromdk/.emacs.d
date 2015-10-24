(require 'req-package)

(req-package windresize
  :config
  (progn
    (defalias 'wr 'windresize)))


(provide 'init-windresize)
