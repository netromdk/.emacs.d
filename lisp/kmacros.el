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
;; Bind last macro to keys: `kmacro-bind-to-key' (C-x C-k b)
;;
;; Store last macro in register R: `kmacro-to-register' (C-x C-k x)
;;
;; Insert macro query into a live macro via `C-x q'. Executing a macro with macro queries will show
;; a prompt for each of them about continuing (`y' or `SPC'), skipping rest of macro (`n' or `DEL'),
;; stop macro entirely (`q' or `RET'), recentering the screen (`C-l'), enter recursive edit (`C-r'),
;; and ending recursive edit (`C-M-c').
;;
;; While recording a macro, a counter (maintained by `kmacro-counter') can be inserted and then
;; incremented by using `kmacro-insert-counter' (C-x C-k TAB). For each execution of the macro, the
;; counters will be inserted and incremented. The counter can be set to a specific value by using
;; `kmacro-set-counter' (C-x C-k C-c) or have a value added to it via `kmacro-add-counter' (C-x C-k
;; C-a). The format of counters is `%d' by default but it can be changed via `kmacro-set-format'
;; (C-x C-k C-f).
;;
;; `kmacro-apply-macro-to-region-lines' applies a macro at the beginning of each line of a region.
;;
;; Enter keyboard macro editor for last macro via `kmacro-edit-macro-repeat' (C-x C-k C-e). The
;; buffer is editable and can be executed using `C-c C-c'. Or edit the last 300 keystrokes as a
;; macro via `kmacro-edit-lossage' (C-x C-k l). Or edit a specific macro via `edit-kbd-macro' (C-x
;; C-k e), selecting the macro either via `C-x e' (last macro), `M-x' (choose the macro to edit), or
;; a key sequence a macro is bound to. A macro can also be step-wise edited via
;; `kmacro-step-edit-macro' (C-x C-k SPC).


;;;; Keyboard macros setup

(require 'kmacro)

;; Add aliases that should have been part of Emacs by default.
(defalias 'kmacro-insert-macro 'insert-kbd-macro)
(defalias 'kmacro-apply-macro-to-region-lines 'apply-macro-to-region-lines)

;; Insert macro by name via `C-x C-k i'.
(define-key kmacro-keymap (kbd "i") #'kmacro-insert-macro)


;;;; Saved keyboard macros

;; Show magit status buffer to the left and magit log to the right.
(fset 'netrom-magit-status-and-log
   (kmacro-lambda-form [?\C-x ?1 ?\C-x ?g ?\C-x ?3 ?\C-x ?o ?l ?l] 0 "%d"))
