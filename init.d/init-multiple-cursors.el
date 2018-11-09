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

    ;; Misc
    ("a" mc/mark-all-like-this-dwim "All" :column "Misc")
    ("m" mc/mark-all-dwim "Dwim")
    ("l" mc/edit-lines "Edit lines" :color blue)
    ("q" nil "Quit" :color blue))

  (global-set-key (kbd "C-c m") 'msk/mc-hydra/body)

  ;; Trigger hydra via some of the normal bindings, too.
  (global-set-key (kbd "C-c n") 'msk/mc-hydra/mc/mark-next-like-this)
  (global-set-key (kbd "C-c p") 'msk/mc-hydra/mc/mark-previous-like-this)
  (global-set-key (kbd "C-c a") 'msk/mc-hydra/mc/mark-all-like-this-dwim))


(provide 'init-multiple-cursors)
