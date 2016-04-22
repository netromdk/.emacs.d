;; Using hippie-expand instead of dabbrev-expand.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Put dabbrev expansions first because it's most often what's expected.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-expand-whole-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-expand-line))

;; Enable the lisp expansion functions for lisp modes.
(add-to-multiple-hooks
 (lambda ()
   (setq hippie-expand-try-functions-list
         '(try-complete-lisp-symbol-partially
           try-complete-lisp-symbol
           try-expand-dabbrev
           try-expand-dabbrev-all-buffers
           try-expand-dabbrev-from-kill
           try-expand-all-abbrevs
           try-expand-whole-kill
           try-complete-file-name-partially
           try-complete-file-name
           try-expand-list
           try-expand-line)))
 '(emacs-lisp-mode-hook
   lisp-interaction-mode-hook))


(provide 'init-hippie-expand)
