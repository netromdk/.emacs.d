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
                  (setq flycheck-clang-language-standard "c++11"
                        flycheck-clang-standard-library "libc++"
                        flycheck-gcc-language-standard "c++11"
                        flycheck-cppcheck-standards '("c++11")
                        flycheck-cppcheck-inconclusive t
                        flycheck-cppcheck-checks '("all")

                        ;; Ignore "no explicit constructor" because often you don't want it to be
                        ;; explicit and in general it's annoying.
                        flycheck-cppcheck-suppressions '("noExplicitConstructor")))))

    ;; Disable elisp checkdoc because it's annoying, and clang/gcc because they never know the
    ;; includes anyway!
    (setq-default flycheck-disabled-checkers
                  '(emacs-lisp-checkdoc c/c++-clang c/c++-gcc))

    (defalias 'fcn 'flycheck-next-error)

    (add-hook 'prog-mode-hook 'flycheck-mode)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
