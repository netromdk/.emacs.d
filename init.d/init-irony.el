(require 'req-package)


;; Compile and install irony server:
;; CXX=clang++-mp-3.8 cmake -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=ON -DCMAKE_PREFIX_PATH=/opt/local/libexec/llvm-3.8 -DCMAKE_INSTALL_PREFIX=~/.emacs.d/irony/ ~/.emacs.d/elpa/irony-20160317.1527/server && cmake --build . --use-stderr --config Release --target install

(req-package irony
  :require company company-irony company-irony-c-headers
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  ;; Replace the `completion-at-point' and `complete-symbol' bindings in irony-mode's buffers by
  ;; irony-mode's function.
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony))

  ;; adds CC special commands to `company-begin-commands' in order to
  ;; trigger completion at interesting places, such as after scope operator
  ;;     std::|
  ;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  )

(req-package flycheck-irony
  :require irony flycheck
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(req-package irony-eldoc
  :require irony
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))


(provide 'init-irony)
