;; "complete anyhting"
(require 'req-package)

(req-package company
  :require company-statistics
  :config
  (setq company-idle-delay 0.3)

  (add-hook 'global-company-mode-hook 'company-statistics-mode)

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

(req-package company-flx
  :require company
  :config
  (company-flx-mode +1))

;; Company-statistics is a global minor mode built on top of the in-buffer completion system
;; company-mode. The idea is to keep a log of a certain number of completions you choose, along with
;; some context information, and use that to rank candidates the next time you have to choose —
;; hopefully showing you likelier candidates at the top of the list.
(req-package company-statistics
  :config
  (setq company-statistics-file
        (concat user-cache-directory "company-statistics-cache.el")))


(provide 'init-company)
