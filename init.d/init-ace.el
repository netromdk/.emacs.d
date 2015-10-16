(require 'req-package)

(req-package ace-jump-mode
  :config
  (progn
    ;; Jump to symbol
    (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

    ;; Jump back to before taking an ace jump
    (autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back" t)
    (eval-after-load "ace-jump-mode"
      '(ace-jump-mode-enable-mark-sync))
    (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)))


(provide 'init-ace)
