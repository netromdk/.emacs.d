;; Keyboard macros
;;
;; New macro:
;;
;; 1) Create macro starting with `kmacro-start-macro' (`C-x (').
;; 2) End it with `kmacro-end-macro' (`C-x )').
;; 3) Name it with `kmacro-name-last-macro' (C-x C-k n).
;; 4) Insert the new macro into this file by invoking `kmacro-insert-macro'.
;;
;; Tricks:
;;
;; Enter keyboard macro editor for last macro via `kmacro-edit-macro-repeat' (C-x C-k C-e). The
;; buffer is editable and can be executed using `C-c C-c'.
;;
;; Bind last macro to keys: `kmacro-bind-to-key' (C-x C-k b)
;;
;; Store last macro in register R: `kmacro-to-register' (C-x C-k x)
;;
;; Insert macro query into a live macro via `C-x q'. Executing a macro with macro queries will show
;; a prompt for each of them about continuing (`y'), skipping rest of macro (`n'), stop macro
;; entirely (`RET'), recentering the screen (`C-l'), enter recursive edit (`C-r'), and ending
;; recursive edit (`C-M-c').


;;;; Keyboard macros setup

(require 'kmacro)

;; Add alias that should have been part of Emacs by default.
(defalias 'kmacro-insert-macro 'insert-kbd-macro)

;; Insert macro by name via `C-x C-k i'.
(define-key kmacro-keymap (kbd "i") #'kmacro-insert-macro)


;;;; Saved keyboard macros

;; Show magit status buffer to the left and magit log to the right.
(fset 'netrom-magit-status-and-log
   (kmacro-lambda-form [?\C-x ?1 ?\C-x ?g ?\C-x ?3 ?\C-x ?o ?l ?l] 0 "%d"))
