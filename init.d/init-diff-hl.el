(require 'req-package)

(req-package diff-hl
  :config
  (progn
    (global-diff-hl-mode 1)
    (diff-hl-flydiff-mode 1)))


(provide 'init-diff-hl)
