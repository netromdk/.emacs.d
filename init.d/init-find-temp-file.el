(require 'req-package)

;; Open a temp file with a certain file extension.
(req-package find-temp-file
  :bind ("C-x C-t" . find-temp-file))

(provide 'init-find-temp-file)
