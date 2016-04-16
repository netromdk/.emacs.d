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

  (defalias 'sb 'ispell-buffer)

  ;; C-x C-i to ispell word and save value as abbrev globally so it can be completed again using
  ;; dabbrev-expand or hippie-expand, for instance. Otherwise it keeps looking for spelling mistakes
  ;; backwards.
  (defun endless/simple-get-word ()
    (car-safe (save-excursion (ispell-get-word nil))))

  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will be global.
If there's nothing wrong with the word at point, keep looking for
a typo until the beginning of buffer. You can skip typos you
don't want to fix with `SPC', and you can abort completely with
`C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (endless/simple-get-word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word)
          (backward-char))
        (setq aft (endless/simple-get-word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point"))))

  (global-set-key (kbd "C-x C-i") 'endless/ispell-word-then-abbrev))

(req-package flyspell
  :require ispell flyspell-lazy
  :config
  ;; Flyspell activation for text mode.
  (add-hook 'text-mode-hook (lambda ()
                              (flyspell-lazy-mode)
                              (flyspell-mode)))

  ;; Remove Flyspell from some sub modes of text mode
  (dolist (hook '(change-log-mode-hook
                  log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  ;; Flyspell comments and strings in programming modes.
  ;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

;; Improve flyspell responsiveness using idle timers.
(req-package flyspell-lazy)

;; Tries to automatically detect the language of the buffer and setting the dictionary accordingly.
(req-package auto-dictionary
  :require ispell
  :config
  (add-hook 'text-mode-hook 'auto-dictionary-mode))


(provide 'init-spelling)
