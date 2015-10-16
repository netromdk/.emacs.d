;;;;;;;;; Luxion related

(defun wrap-luxion-count (start end)
  "Determines the longest horizontal stretch of the function selected."
  (progn
    (goto-char start)
    (let (max)
      (progn 
        (setq max 0)
        (while (search-forward "\n" end 't)
          (progn
            ;; Move back once to get the correct column. The cursor is
            ;; on the first char on the next line after searching for
            ;; "\n".
            (backward-char) 
            (if (> (current-column) max)
                (setq max (current-column)))
            (forward-char))))
      ;; Subtract three to compensate for "// ".
      (- max 3))))

;; Luxion: Wraps // ***.. before and after a function (the region
;; selected). Place region at the start of the function and end it
;; after "{" on the next line.
(defun wrap-luxion-function (start end)
 "Put comments around Luxion function."
 (interactive "r")    
 (let (count str)
   (progn
     (save-excursion 
       (setq count (wrap-luxion-count start end))
       (setq str (concat "// " (make-string count ?*) "\n"))
       (goto-char end) 
       (insert str)
       (goto-char start) 
       (insert str)))))

(global-set-key (kbd "C-M-l") 'wrap-luxion-function)


(provide 'init-luxion)
