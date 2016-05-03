;; "complete anyhting"
(require 'req-package)

(req-package company
  :config
  ;;(add-to-list 'company-backends 'company-c-headers)
  (global-company-mode 1)

  ;; Use C-tab globally for company-complete but use C-tab for gtags
  ;; completion locally for C like modes, but in that case keep C-return
  ;; with the default completion.
  (global-set-key (kbd "C-<tab>") 'company-complete)

  ;; Disabled because irony is working better for now.
  ;; (add-hook 'c-mode-common-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
  ;;               (local-set-key (kbd "C-<tab>") 'company-gtags)
  ;;               (local-set-key (kbd "C-<return>") 'company-complete))))
  )

(req-package company-c-headers
  :require company)


(provide 'init-company)
