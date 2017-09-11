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
  (global-set-key (kbd "C-y") #'hydra-yank-pop/yank))

(provide 'init-hydra)
