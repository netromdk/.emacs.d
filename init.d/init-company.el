;; "complete anyhting"
(require 'req-package)

(req-package company
  :require company-statistics
  :config
  (setq company-idle-delay 0.3)

  (add-hook 'global-company-mode-hook 'company-statistics-mode)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

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
        (concat user-cache-dir "company-statistics-cache.el")))

(req-package company-lsp
  :require company
  :config
  (push 'company-lsp company-backends)

  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil))

(req-package company-cmake
  :config
  (push 'company-cmake company-backends)
  (setq company-cmake-executable (executable-find "cmake")))


(provide 'init-company)
