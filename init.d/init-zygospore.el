;; Restore buffers.
(require 'req-package)

(req-package zygospore
  :config
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))


(provide 'init-zygospore)
