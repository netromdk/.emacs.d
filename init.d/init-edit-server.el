(require 'req-package)


;; Server that responds to edit requests from Chrome.
(req-package edit-server
  :config
  (setq edit-server-new-frame t
        edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)
          ("git\\.luxion\\.dk" . markdown-mode)))

  ;; Dimensions (in characters) of editing frame.
  (add-to-list 'edit-server-new-frame-alist '(width  . 100))
  (add-to-list 'edit-server-new-frame-alist '(height . 24))

  ;; Give focus back to Chrome when done editing.
  (with-system darwin
    (add-hook 'edit-server-done-hook
              (lambda () (shell-command "open -a \"Google Chrome\""))))

  (edit-server-start))


(provide 'init-edit-server)
