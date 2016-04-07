(require 'req-package)

(req-package ispell
  :config
  ;; Set aspell as spell program
  (setq ispell-program-name "aspell")

  ;; Speed up aspell: ultra | fast | normal
  (setq ispell-extra-args '("--sug-mode=normal"))

  (defun da-spell ()
    "Set ispell to use Danish dictionary (locally)"
    (interactive)
    (ispell-change-dictionary "dansk"))

  (defun en-spell ()
    "Set ispell to use English dictionary (locally)"
    (interactive)
    (ispell-change-dictionary "english"))

  (defalias 'sb 'ispell-buffer))

(req-package flyspell
  :require ispell
  :config
  ;; Flyspell activation for text mode.
  (add-hook 'text-mode-hook 'flyspell-mode)

  ;; Remove Flyspell from some sub modes of text mode
  (dolist (hook '(change-log-mode-hook
                  log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  ;; Flyspell comments and strings in programming modes.
  ;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

;; Tries to automatically detect the language of the buffer and setting the dictionary accordingly.
(req-package auto-dictionary
  :require ispell
  :config
  (add-hook 'text-mode-hook 'auto-dictionary-mode))


(provide 'init-spelling)
