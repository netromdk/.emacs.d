(require 'req-package)

(req-package yasnippet
  :config
  (progn
    (setq yas-snippet-dirs '((concat user-emacs-directory "snippets")))
    (add-hook 'prog-mode-hook 'yas-minor-mode)))


(provide 'init-yasnippet)
