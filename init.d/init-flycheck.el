;;; init-flycheck --- Summary
;;; Commentary:
;;; Code:
(require 'req-package)

(req-package flycheck
  :config
  (progn
    ;; C++11
    (add-hook 'c++-mode-hook
              (lambda ()
                (progn
                  (setq flycheck-clang-language-standard "c++11")
                  (setq flycheck-clang-standard-library "libc++")
                  (setq flycheck-gcc-language-standard "c++11"))))

    ;; Disable elisp checkdoc because it's annoying!
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

    (add-hook 'prog-mode-hook 'flycheck-mode)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
