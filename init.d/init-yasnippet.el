(require 'req-package)

(req-package yasnippet
  :config
  (progn
    ;; Add local snippets to override some of the defaults in elpa folder.
    (add-to-list 'yas-snippet-dirs yas-dir)

    ;; Use fixed indentation so the snippets look as they were typed!
    (setq yas-indent-line 'fixed)

    (yas-global-mode 1)

    ;; Disable normal tab expansion because it often interferes with
    ;; indentation.
    (define-key yas-minor-mode-map (kbd "<tab>") nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-M-y") 'yas-expand)))


(provide 'init-yasnippet)
