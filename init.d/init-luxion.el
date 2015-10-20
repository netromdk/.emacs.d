;;;;;;;;; Luxion related

;; Wraps a function with // ***.. before and after (the region selected). Both
;; inserted lines with have a length fo 80 characters.
(defun wrap-luxion-function (start end)
  "Put comments around Luxion function."
  (interactive "r")
  (let (count str)
    (progn
      (save-excursion
        (setq count 77) ;; 80 - "// "
        (setq str (concat "// " (make-string count ?*) "\n"))
        (goto-char end)
        (insert str)
        (goto-char start)
        (insert str)))))

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-M-l") 'wrap-luxion-function)))


(provide 'init-luxion)
