(require 'req-package)

;; Sometimes the cursor should be at the opposite end of the search match, e.g. when searching
;; forward the cursor will be at the end of the match, but if the opposite is intended then exit
;; isearch mode using C-RET. Same thing for searching backwards where it puts the cursor at the end
;; of the match instead.
;; (defun isearch-exit-other-end ()
;;   "Exit isearch, at the opposite end of the string."
;;   (interactive)
;;   (isearch-exit)
;;   (goto-char isearch-other-end))
;; (define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; (defun zap-to-isearch (beg end)
;;   "Kill the region between the mark and the closest portion of
;; the isearch match string."
;;   (interactive "r")
;;   (when (not mark-active)
;;     (error "Mark is not active"))
;;   (save-excursion
;;     (let* ((isearch-bounds (list isearch-other-end (point)))
;;            (ismin (apply 'min isearch-bounds))
;;            (ismax (apply 'max isearch-bounds)))
;;       (if (< (mark) ismin)
;;           (kill-region (mark) ismin)
;;         (if (> (mark) ismax)
;;             (kill-region ismax (mark))
;;           (error "Internal error in isearch kill function.")))
;;       (isearch-exit))))

;; (define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)

(req-package avy
  :require (hydra dumb-jump)
  :config
  (defhydra avy-hydra (:color blue :columns 3)
    "avy-goto"
    ("g" avy-goto-line "Line")
    ("M-g" avy-goto-line "Line")
    ("p" avy-goto-line-above "Line above")
    ("n" avy-goto-line-below "Line below")
    ("c" avy-goto-char-2 "Char-2")
    ("C" avy-goto-char "Char")
    ("w" avy-goto-word-1 "Word")
    ("s" avy-goto-subword-1 "Subword")
    ("l" avy-goto-char-in-line "Char in line")
    ("," avy-pop-mark "Pop mark")
    ("d" dumb-jump-hydra/body "Dumb jump hydra"))
  (global-set-key (kbd "M-g") 'avy-hydra/body))

;; ace-isearch combines ace/avy, isearch and help-swoop. Typing one char will invoke ace/avy, typing
;; more searches normally with isearch, and 6 or more invokes helm-swoop.
(req-package ace-isearch
  :require helm-swoop avy ace-jump-mode
  :config
  (setq ace-isearch-input-idle-jump-delay 1.0
        ace-isearch-function 'avy-goto-char
        ace-isearch-input-length 7 ; Invoke helm-swoop when >= 7.
        ace-isearch-function-from-isearch 'ace-isearch-helm-swoop-from-isearch
        ace-isearch-use-jump 'printing-char)
  (global-ace-isearch-mode +1)

  ;; Don't use ace-isearch when defining a macro because it triggers one-char ace mode immediately.
  (defadvice ace-isearch--jumper-function (around ace-isearch--jumper-function-kmacro-advice)
    (unless (or defining-kbd-macro executing-kbd-macro)
      ad-do-it))
  (ad-activate 'ace-isearch--jumper-function))

(provide 'init-search)
