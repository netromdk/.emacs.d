;; (require 'req-package)


;; ;; Compile and install irony server:
;; ;; CXX=clang++-mp-3.8 cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON -DCMAKE_PREFIX_PATH=/opt/local/libexec/llvm-3.8 -DCMAKE_INSTALL_PREFIX=~/.emacs.d/irony/ ~/.emacs.d/elpa/irony-20160317.1527/server && cmake --build . --use-stderr --config Release --target install

;; (req-package irony
;;   :require company company-irony company-irony-c-headers helm
;;   :config
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)

;;   (setq my-irony-cdb-loaded-time nil)
;;   (defun my-irony-mode-hook ()
;;     ;; Replace the `completion-at-point' and `complete-symbol' bindings in irony-mode's buffers by
;;     ;; irony-mode's function.
;;     (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async)

;;     ;; Only run auto-setup first time to be faster. However, it's not perfect because if opening a
;;     ;; file in a different project root it will not auto-setup for the new CDB. But then use
;;     ;; `irony-cdb-json-select'.
;;     (when (not my-irony-cdb-loaded-time)
;;       (setq my-irony-cdb-loaded-time (current-time))
;;       (message "Irony CDB auto-setup...")
;;       (irony-cdb-autosetup-compile-options)
;;       (show-elapsed-time "Irony CDB auto-setup done in" my-irony-cdb-loaded-time (current-time))))

;;   ;;(add-hook 'irony-mode-hook 'my-irony-mode-hook)

;;   (eval-after-load 'company
;;     '(add-to-list 'company-backends 'company-irony))

;;   ;; adds CC special commands to `company-begin-commands' in order to
;;   ;; trigger completion at interesting places, such as after scope operator
;;   ;;     std::|
;;   ;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;   ;; Put advice around `irony-cdb-json-select' to use a helm prompt.
;;   (defadvice irony-cdb-json-select (around advice-irony-cdb-json-select activate)
;;     "Select CDB using helm prompt."
;;     (interactive)
;;     (let ((on (bound-and-true-p helm-mode)))
;;       (when (not on) (helm-mode 1))
;;       (unwind-protect
;;           ad-do-it
;;         (when (not on) (helm-mode -1))))))

;; (req-package flycheck-irony
;;   :require irony flycheck
;;   :config
;;   (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

;; (req-package irony-eldoc
;;   :require irony
;;   :config
;;   (add-hook 'irony-mode-hook 'irony-eldoc))


;; (provide 'init-irony)
