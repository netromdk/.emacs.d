;; Visualize certain like space at end of line and trailing characters after
;; fill column.
(setq whitespace-style '(face empty tabs lines-tail trailing))

;; Enable whitespace for programming modes.
(add-hook 'prog-mode-hook 'whitespace-mode)

(provide 'init-whitespace)
