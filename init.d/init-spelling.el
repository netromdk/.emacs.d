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

(req-package auto-dictionary)

(req-package flyspell
  :require ispell auto-dictionary
  :config
  ;; Flyspell activation for text mode, and try to automatically detect the language of the buffer.
  (add-hook 'text-mode-hook
            (lambda ()
              (flyspell-mode)
              (auto-dictionary-mode)))

  ;; Remove Flyspell from some sub modes of text mode
  (dolist (hook '(change-log-mode-hook
                  log-edit-mode-hook))
    (add-hook hook (lambda ()
                     (flyspell-mode -1)
                     (auto-dictionary-mode -1))))

  ;; Flyspell comments and strings in programming modes.
  ;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )


(provide 'init-spelling)
