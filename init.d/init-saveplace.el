;; Saves the buffer positions and restores them.
(require 'req-package)

(req-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file "~/.emacs.d/saveplace.txt")))


(provide 'init-saveplace)
