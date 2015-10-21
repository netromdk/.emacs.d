;; Visualize certain like space at end of line and trailing characters after
;; fill column.
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column global-fill-column)

(add-hook 'prog-mode-hook 'whitespace-mode)


(provide 'init-whitespace)
