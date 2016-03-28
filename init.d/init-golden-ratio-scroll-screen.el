(require 'req-package)

(req-package golden-ratio-scroll-screen
  :config
  (progn
    (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
    (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up)))


(provide 'init-golden-ratio-scroll-screen)
