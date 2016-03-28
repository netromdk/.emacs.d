(require 'req-package)

(req-package linum
  :config
  (progn
    ;; Show line number mode only while using goto-line.
    (defun goto-line-with-feedback ()
      "Show line numbers temporarily, while prompting for the line number input"
      (interactive)
      (let ((line-numbers-off-p (not linum-mode)))
        (unwind-protect
            (progn (when line-numbers-off-p
                     (linum-mode 1))
                   (call-interactively 'goto-line))
          (when line-numbers-off-p
            (linum-mode -1)))))

    ;; Since M-g is a prefix and hardly used it is mapped to
    ;; goto-line-with-feedback directly. If only wanting to remap the normal
    ;; binding then use [remap goto-line] instead of (kbd "M-g").
    (global-set-key (kbd "M-g") 'goto-line-with-feedback)))

;; Disable right now because the line numbers are only shown when using M-g.
;; (req-package hlinum
;;   :config
;;   (progn
;;     ;; highlights current line number in margin
;;     (hlinum-activate)))

(req-package highlight-current-line
  :config
  (progn
    (highlight-current-line-minor-mode)
    (highlight-current-line-on t)))


(provide 'init-line-numbers)
