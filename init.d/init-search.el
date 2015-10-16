(require 'req-package)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(req-package flx-isearch
  :require flx
  :config
  (progn
    (global-set-key (kbd "C-M-s") 'flx-isearch-forward)
    (global-set-key (kbd "C-M-r") 'flx-isearch-backward)))


(provide 'init-search)
