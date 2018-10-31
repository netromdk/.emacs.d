;;; init-flycheck --- Summary
;;; Commentary:
;;; Code:
(require 'req-package)

(req-package flycheck
  :require (helm-flycheck hydra)
  :config
  (progn
    ;; C++11
    (add-hook 'c++-mode-hook
              (lambda ()
                (progn
                  (setq flycheck-clang-language-standard "c++14"
                        flycheck-clang-standard-library "libc++"
                        flycheck-gcc-language-standard "c++14"
                        flycheck-cppcheck-standards '("c++14")
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
    (defalias 'hf 'helm-flycheck)

    (add-hook 'prog-mode-hook 'flycheck-mode)

    ;; Navigate flycheck errors more easily.
    (defhydra flycheck-hydra
      (:pre  (flycheck-list-errors)
             :post (quit-windows-on "*Flycheck errors*")
             :hint nil)
      "Errors"
      ("f"  flycheck-error-list-set-filter                            "Filter")
      ("j"  flycheck-next-error                                       "Next")
      ("k"  flycheck-previous-error                                   "Previous")
      ("gg" flycheck-first-error                                      "First")
      ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
      ("q"  nil))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
