(require 'req-package)

(req-package define-word
  :config
  (progn
    ;; Show definitions of a word at point.
    (global-set-key (kbd "C-c ?") 'define-word-at-point)))


(provide 'init-define-word)
