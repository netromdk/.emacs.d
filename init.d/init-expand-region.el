(require 'req-package)

(req-package expand-region
  :config
  (progn
    ;; Expand region to select text intelligently.
    (global-set-key (kbd "C-<") 'er/expand-region)
    (global-set-key (kbd "C->") 'er/contract-region)))


(provide 'init-expand-region)
