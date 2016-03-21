(require 'req-package)

(req-package spaceline
  :config
  (progn
    (window-numbering-mode t)

    (require 'spaceline-config)
    (spaceline-spacemacs-theme)

    ;; Don't show unicode window numbers because they are too small to be seen
    ;; fast and clearly.
    (setq spaceline-window-numbers-unicode nil)

    (setq spaceline-minor-modes-separator " ")

    (spaceline-helm-mode)

    (spaceline-toggle-process-on)
    (spaceline-toggle-selection-info-on)
    (spaceline-toggle-hud-off)))


(provide 'init-spaceline)
