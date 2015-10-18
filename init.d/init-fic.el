(require 'req-package)

;; Marks TODO, FIXME etc. clearly.
(req-package fic-mode
  :init
  (progn
    (setq fic-background-color "#ff9800")
    (setq fic-foreground-color "#000000")
    (setq fic-highlighted-words
          (quote ("FIX" "FIXME" "TODO" "BUG" "KLUDGE" "TMP" "TEMP"))))

  :config
  (progn
    (add-hook 'prog-mode-hook 'fic-mode)))


(provide 'init-fic)
