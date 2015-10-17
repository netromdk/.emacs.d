(require 'req-package)

(req-package markdown-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

    ;; Turn off auto-fill-mode beacuse markdown is sensitive about newlines.
    (add-hook 'markdown-mode-hook
              (lambda ()
                (auto-fill-mode 0)
                (visual-line-mode t)))))


(provide 'init-markdown)
