(require 'req-package)

;; Marks TODO, FIXME etc. clearly.
(req-package fic-mode
  :config
  (add-hook 'prog-mode-hook 'fic-mode))


(provide 'init-fic)
