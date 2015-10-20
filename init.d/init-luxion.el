;;;;;;;;; Luxion related

;; Wraps a function with // ***.. before and after (the region selected). Both
;; inserted lines with have a length fo 80 characters.
(defun wrap-luxion-function (start end)
  "Put comments around Luxion function."
  (interactive "r")
  (let ((str (concat "// " (make-string 77 ?*) "\n")))
    (save-excursion
      (goto-char end)
      (insert str)
      (goto-char start)
      (insert str))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-M-l") 'wrap-luxion-function)))

(defun fix-luxion-buffer ()
  "Fix all functions with an incorrect number of '// ***..' around them."
  (interactive)
  (let* ((regexp "[ ]*\/\/[ ]*[\*]+")
         (line-width 80)
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


(provide 'init-luxion)
