;;; init-flycheck --- Summary
;;; Commentary:
;;; Code:
(require 'req-package)

(req-package flycheck
  :require helm-flycheck hydra flycheck-pycheckers
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

;; Requires local dependencies:
;;   pip install flake8 bandit
(req-package flycheck-pycheckers
  :config
  (setq flycheck-pycheckers-checkers '(flake8 bandit)
        flycheck-pycheckers-ignore-codes
        '("C0411" "C0413" "C0103" "C0111" "W0142" "W0201" "W0232" "W0403" "W0511" "E1002" "E1101"
          "E1103" "R0201" "R0801" "R0903" "R0904" "R0914" "W503" "W504"
          ;; flake8
          "E111" "E114" "E121" "E126" "E127" "E221" "E241" "E302" "E305"
          ;; bandit
          "B101" "B322")
        flycheck-pycheckers-max-line-length 100
        flycheck-pycheckers-multi-thread "true")

  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)

  ;; Ensure that the correct python checker is chosen.
  (add-hook 'python-mode-hook (lambda () (flycheck-select-checker 'python-pycheckers))))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
