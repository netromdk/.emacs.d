(require 'req-package)

;; Sometimes the cursor should be at the opposite end of the search match, e.g. when searching
;; forward the cursor will be at the end of the match, but if the opposite is intended then exit
;; isearch mode using C-RET. Same thing for searching backwards where it puts the cursor at the end
;; of the match instead.
(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))
(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(defun zap-to-isearch (beg end)
  "Kill the region between the mark and the closest portion of
the isearch match string."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (save-excursion
    (let* ((isearch-bounds (list isearch-other-end (point)))
           (ismin (apply 'min isearch-bounds))
           (ismax (apply 'max isearch-bounds)))
      (if (< (mark) ismin)
          (kill-region (mark) ismin)
        (if (> (mark) ismax)
            (kill-region ismax (mark))
          (error "Internal error in isearch kill function.")))
      (isearch-exit))))

(define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)

(req-package flx-isearch
  :require flx
  :config
  (global-set-key (kbd "C-M-s") 'flx-isearch-forward)
  (global-set-key (kbd "C-M-r") 'flx-isearch-backward))

(req-package avy
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char)
  (global-set-key (kbd "C-,") 'avy-goto-char-2)
  (global-set-key [remap goto-line] 'avy-goto-line) ; Enter 1-9 for line mode.
  (global-set-key (kbd "M-g p") 'avy-goto-line-above)
  (global-set-key (kbd "M-g n") 'avy-goto-line-below)
  (global-set-key (kbd "M-g c") 'avy-goto-char-in-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1))


(provide 'init-search)
