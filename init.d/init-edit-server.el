(require 'req-package)


;; Server that responds to edit requests from Chrome.
(req-package edit-server
  :config
  (setq edit-server-new-frame t
        edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)
          ("git\\.luxion\\.dk" . markdown-mode)))
  (edit-server-start))


(provide 'init-edit-server)
