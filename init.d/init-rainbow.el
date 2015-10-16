(require 'req-package)

;; Highlights hexcolors, like #aabbcc and Red.
(req-package rainbow-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-mode)))

;; For programming modes, show delimiters with variying colors to easily
;; distinguish between them.
(req-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))


(provide 'init-rainbow)
