;;;;;;;;; Luxion related

;; Note: bindings are at the bottom of this file!

;; Wraps a function with // ***.. before and after (the region selected). Both
;; inserted lines with have a length fo 80 characters.
(defun lux-wrap-function (start end)
  "Put comments around Luxion function."
  (interactive "r")
  (let ((str (concat "// " (make-string (- global-fill-column 3) ?*) "\n")))
    (save-excursion
      (goto-char end)
      (insert str)
      (goto-char start)
      (insert str))))

(defun lux-fix-function-comments ()
  "Fix all functions with an incorrect number of '// ***..' (or '=' or '-') around them."
  (interactive)
  (let* ((regexp "[ ]*\/\/[ ]*[\*\=\-]+")
         (line-width global-fill-column)
         (str (concat "// " (make-string (- line-width 3) ?*)))
         (old-line)
         (line-end))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at regexp)
          ;; Replace with correct line.
          (beginning-of-line)
          (kill-line)
          (insert str)

          ;; Indent to fit with sorrounded scopes, if any.
          (indent-for-tab-command)

          ;; If the line exceeds `line-width` then kill the rest of line.
          (end-of-line)
          (setq line-end (current-column))
          (beginning-of-line)
          (setq old-line (line-number-at-pos))
          (forward-char line-width)
          (when (and (< (current-column) line-end)
                     (= old-line (line-number-at-pos)))
            (kill-line))
          (goto-line old-line))
        (forward-line))))) ;; Search next line.

(defun lux-fix-function-curls ()
  "Fix all functions with '// ***..' around it to have it's '{' be put after the second '//***..'."
  (interactive)
  (let* ((regexp-line "[ ]*\/\/[ ]*[\*]+")
         (regexp-curl "{")
         (line-width global-fill-column)
         (str (concat "// " (make-string (- line-width 3) ?*)))
         (line-one)
         (line-two)
         (flag nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not flag))
        ;; Find line one.
        (while (and (not (eobp))
                    (not flag))
          (when (looking-at regexp-line)
            (setq line-one (line-number-at-pos))
            (setq flag t))
          (forward-line))

        (unless (eobp)
          (setq flag nil)
          (forward-line)

          ;; Find line two.
          (while (and (not (eobp))
                      (not flag))
            (when (looking-at regexp-line)
              (setq line-two (line-number-at-pos))
              (setq flag t))
            (forward-line))

          (unless (eobp)
            (setq flag nil)
            (goto-line line-one)

            ;; Find and remove the '{'.
            (while (and (<= (line-number-at-pos) line-two)
                        (not flag))
              (when (looking-at regexp-curl)
                (delete-char 1)
                (cycle-spacing 0) ;; Remove any whitespace
                (setq flag t))
              (forward-char 1))

            (unless (eobp)
              (goto-line line-two)

              ;; If deleted '{' then insert on new line after `line-two`.
              (when flag
                (end-of-line)
                (insert "\n")
                (insert regexp-curl))

              (setq flag nil)
              (forward-line))))))))

(defun lux-fix-buffer ()
  (interactive)
  (lux-fix-function-curls)
  (cleanup-region-or-buffer)
  (lux-fix-function-comments)
  (clang-format-buffer))

;; Bindings
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-M-l") 'lux-wrap-function)
            (local-set-key (kbd "C-c l") 'lux-fix-buffer)))


(provide 'init-luxion)
