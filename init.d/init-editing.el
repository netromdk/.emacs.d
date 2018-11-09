(require 'req-package)

;; Convenient word transformations.
(req-package fix-word
  :config
  (global-set-key (kbd "M-u") 'fix-word-upcase)
  (global-set-key (kbd "M-l") 'fix-word-downcase)
  (global-set-key (kbd "M-c") 'fix-word-capitalize))


(provide 'init-editing)
