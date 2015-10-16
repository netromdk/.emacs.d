(require 'req-package)

(req-package fic-mode
  :config
  (progn
    ;; Marks TODO, FIXME etc. clearly.
    (setq fic-background-color "#ff9800")
    (setq fic-foreground-color "#000000")
    (setq fic-highlighted-words (quote ("FIXME" "TODO" "BUG" "KLUDGE" "TEMP")))

    (add-hook 'prog-mode-hook 'fic-mode)))


(provide 'init-fic)
