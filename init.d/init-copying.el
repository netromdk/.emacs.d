(require 'req-package)

;; Convenience for formatting things to be copied and inserted elsewhere, like code blocks.
(req-package copy-as-format
  :config
  (global-set-key (kbd "C-c w s") 'copy-as-format-slack)
  (global-set-key (kbd "C-c w m") 'copy-as-format-markdown)
  (global-set-key (kbd "C-c w g") 'copy-as-format-github)
  (global-set-key (kbd "C-c w l") 'copy-as-format-gitlab)
  (global-set-key (kbd "C-c w h") 'copy-as-format-html)
  (global-set-key (kbd "C-c w j") 'copy-as-format-jira))

(provide 'init-copying)
