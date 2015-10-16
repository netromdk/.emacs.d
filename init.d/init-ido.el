(require 'req-package)

(req-package flx-ido
  :require flx
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)

    ;; Disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))


(provide 'init-ido)
