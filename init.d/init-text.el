;; Set auto-fill-mode and org minor modes for lists and tables.
(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode t)
            (orgstruct-mode t)
            (orgtbl-mode t)))

(provide 'init-text)
