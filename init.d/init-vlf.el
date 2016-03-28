(require 'req-package)

;; Using very-large-file mode for large files without asking.
(req-package vlf
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))


(provide 'init-vlf)
