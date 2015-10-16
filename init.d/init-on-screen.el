;; on-screen mode to track reading markers.
(require 'req-package)

(req-package on-screen
  :config
  (progn
    (setq on-screen-highlight-method (quote fringe))
    (global-on-screen-mode 1)))


(provide 'init-on-screen)
