(require 'req-package)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(req-package flx-isearch
  :require flx
  :config
  (global-set-key (kbd "C-M-s") 'flx-isearch-forward)
  (global-set-key (kbd "C-M-r") 'flx-isearch-backward))

(req-package avy
  :config
  (global-set-key (kbd "C-c 1") 'avy-goto-char)
  (global-set-key (kbd "C-c 2") 'avy-goto-char-2))


(provide 'init-search)
