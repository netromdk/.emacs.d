(require 'req-package)

(req-package yasnippet
  :config
  (progn
    (yas-global-mode 1)

    ;; Disable normal tab expansion because it often interferes with
    ;; indentation.
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-M-y") 'yas-expand)))


(provide 'init-yasnippet)
