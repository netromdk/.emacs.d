(require 'req-package)

(req-package beacon
  :config
  (setq beacon-blink-delay 0.1
        beacon-blink-duration 0.3
        beacon-color "#b1d631")

  (defun backward-paragraph-blink ()
    (interactive)
    (backward-paragraph)
    (beacon-blink))

  (defun forward-paragraph-blink ()
    (interactive)
    (forward-paragraph)
    (beacon-blink))

  (global-set-key (kbd "M-p") 'backward-paragraph-blink)
  (global-set-key (kbd "M-n") 'forward-paragraph-blink)

  (beacon-mode 1))

;; General bindings.
(global-set-key (kbd "C-c o")
                '(lambda () (interactive) (ff-find-other-file nil t)))
(global-set-key (kbd "C-.") 'repeat)

;; Move to first whitespace or begninning of line if none. Pressing again goes to the beginning if
;; there was whitespace.
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; Cycle through "just one space", "no spaces" and original number of spaces,
;; instead of just "just one space". It does not delete newlines, too.
(global-set-key (kbd "M-SPC")
                '(lambda () (interactive) (cycle-spacing +1 t)))

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; Indent, untabify and clean whitespace of region or buffer.
(global-set-key (kbd "C-c c") 'cleanup-region-or-buffer)

;; Intelligent line opening that also places cursor on new line.
(global-set-key (kbd "C-o") 'prelude-smart-open-line)
(global-set-key (kbd "M-o") 'open-line) ;; Default way.


(provide 'init-bindings)
