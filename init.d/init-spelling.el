(require 'req-package)

(req-package ispell
  :config
  (progn
    ;; Set aspell as spell program
    (setq ispell-program-name "aspell")

    ;; Speed up aspell: ultra | fast | normal
    (setq ispell-extra-args '("--sug-mode=normal"))

    ;; Flyspell activation for text mode
    ;;(add-hook 'text-mode-hook
    ;;          (lambda () (flyspell-mode t)))

    ;; Remove Flyspell from some sub modes of text mode
    ;;(dolist (hook '(change-log-mode-hook
    ;;                log-edit-mode-hook))
    ;;  (add-hook hook (lambda () (flyspell-mode -1))))

    (defun da-spell ()
      "Set ispell to use Danish dictionary (globally)"
      (interactive)
      (ispell-change-dictionary "dansk" "global"))

    (defun en-spell ()
      "Set ispell to use English dictionary (globally)"
      (interactive)
      (ispell-change-dictionary "english" "global"))

    (defalias 'sb 'ispell-buffer)))


(provide 'init-spelling)
