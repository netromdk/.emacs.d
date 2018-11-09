(require 'req-package)

;; Convenient word transformations.
(req-package fix-word
  :config
  (global-set-key (kbd "M-u") 'fix-word-upcase)
  (global-set-key (kbd "M-l") 'fix-word-downcase)
  (global-set-key (kbd "M-c") 'fix-word-capitalize))

;; Better fill/unfill that is toggleable.
(req-package unfill
  :config
  (global-set-key (kbd "M-q") 'unfill-toggle))


(provide 'init-editing)
