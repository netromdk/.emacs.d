(require 'req-package)

;; Lots of inspiration found here: https://github.com/abo-abo/hydra/wiki/
(req-package hydra
  :require helm
  :config

  ;; Easier cycling of yanking.
  (defhydra hydra-yank-pop ()
    "yank"
    ("C-y" yank nil)
    ("M-y" yank-pop nil)
    ("y" (yank-pop 1) "next")
    ("Y" (yank-pop -1) "prev")
    ("l" helm-show-kill-ring "list" :color blue))

  (global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
  (global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

  ;; Easy nagivation without holding down the control all the time.
  (defhydra hydra-move ()
    "move"
    ("f" forward-char)
    ("F" forward-word)
    ("n" next-line)
    ("N" forward-paragraph)

    ("b" backward-char)
    ("B" backward-word)
    ("p" previous-line)
    ("P" backward-paragraph)

    ("a" smarter-move-beginning-of-line)
    ("e" move-end-of-line)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("v" scroll-up-command)
    ("V" scroll-down-command)
    ("l" recenter-top-bottom))

  (global-set-key (kbd "C-f") #'hydra-move/forward-char)
  (global-set-key (kbd "M-f") #'hydra-move/forward-word)
  (global-set-key (kbd "C-n") #'hydra-move/next-line)
  (global-set-key (kbd "M-n") #'hydra-move/forward-paragraph)

  (global-set-key (kbd "C-b") #'hydra-move/backward-char)
  (global-set-key (kbd "M-b") #'hydra-move/backward-word)
  (global-set-key (kbd "C-p") #'hydra-move/previous-line)
  (global-set-key (kbd "M-p") #'hydra-move/backward-paragraph)

  (global-set-key (kbd "C-a") #'hydra-move/smarter-move-beginning-of-line)
  (global-set-key (kbd "C-e") #'hydra-move/move-end-of-line)
  (global-set-key (kbd "M-<") #'hydra-move/beginning-of-buffer)
  (global-set-key (kbd "M->") #'hydra-move/end-of-buffer)
  (global-set-key (kbd "C-v") #'hydra-move/scroll-up-command)
  (global-set-key (kbd "M-v") #'hydra-move/scroll-down-command)
  (global-set-key (kbd "C-l") #'hydra-move/recenter-top-bottom))

(provide 'init-hydra)
