;; Multiple cursors
(require 'req-package)

(req-package multiple-cursors
  :require hydra
  :config
  (setq mc/mode-line
        (quote
         (" ["
          (:eval
           (format
            #("%d" 0 2
              (face font-lock-string-face))
            (mc/num-cursors)))
          " mc]")))

  (global-set-key (kbd "C-c l") 'mc/edit-lines)

  (defhydra msk/mc-hydra ()
    "
Multiple Cursors

"
    ;; Next
    ("n" mc/mark-next-like-this "Next" :column "Next")
    ("u" mc/unmark-next-like-this "Unmark")
    ("s" mc/skip-to-next-like-this "Skip")

    ;; Previous
    ("p" mc/mark-previous-like-this "Previous" :column "Previous")
    ("U" mc/unmark-previous-like-this "Unmark")
    ("S" mc/skip-to-previous-like-this "Skip")

    ;; Cycling
    ;; NOTE: It is in purpose that "C-s" `isearch-forward' and "C-r" `isearch-backward' are replaced
    ;; because isearch isn't supported in mc so it might as well do something familiar!
    ("C-s" mc/cycle-forward "Forward" :column "Cycling")
    ("C-r" mc/cycle-backward "Backward")

    ;; Insert
    ("C-n" mc/insert-numbers "Numbers" :column "Insert")
    ("C-l" mc/insert-letters "Letters")

    ;; "Like This"
    ("a" mc/mark-all-like-this-dwim "All (dwim)" :column "Like This")
    ("d" mc/mark-all-like-this-in-defun "All (defun)")
    ("w" mc/mark-all-words-like-this-in-defun "All words (defun)")
    ("M-s" mc/mark-all-symbols-like-this-in-defun "All symbols (defun)")

    ;; Misc
    ("m" mc/mark-all-dwim "All (dwim)" :column "Misc")
    ("l" mc/edit-lines "Edit lines")
    ("q" nil "Quit" :color blue))

  (global-set-key (kbd "C-c m") 'msk/mc-hydra/body)

  ;; Trigger hydra via some of the normal bindings, too.
  (global-set-key (kbd "C-c n") 'msk/mc-hydra/mc/mark-next-like-this)
  (global-set-key (kbd "C-c p") 'msk/mc-hydra/mc/mark-previous-like-this)
  (global-set-key (kbd "C-c a") 'msk/mc-hydra/mc/mark-all-like-this-dwim)
  (global-set-key (kbd "C-c l") 'msk/mc-hydra/mc/edit-lines))


(provide 'init-multiple-cursors)
