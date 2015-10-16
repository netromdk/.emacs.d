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

    (global-set-key [remap goto-line] 'goto-line-with-feedback)))

(req-package hlinum
  :config
  (progn
    ;; highlights current line number in margin
    (hlinum-activate)))

(req-package highlight-current-line
  :config
  (progn
    (highlight-current-line-minor-mode)
    (highlight-current-line-on t)))


(provide 'init-line-numbers)
