;;;;;;;;; CUSTOM KEYBINDINGS

(global-set-key (kbd "M-n") 'scroll-up-one-line)
(global-set-key (kbd "M-p") 'scroll-down-one-line)
(global-set-key (kbd "M-P") 'previous-user-buffer)
(global-set-key (kbd "M-N") 'next-user-buffer)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Cycle through "just one space", "no spaces" and original number of spaces,
;; instead of just "just one space". It does not delete newlines, too.
(global-set-key (kbd "M-SPC")
                '(lambda () (interactive) (cycle-spacing +1 t)))

;;;;;;;;; FUNCTIONS

;; convert current buffer to unix EOLs
(defun to-unix-eol ()
  "Change current buffer's line ending to unix convention."
  (interactive)
  (progn
    (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos
    (save-buffer)))

(defun open-with-finder ()
  "Show current buffer-file, or directory if in Dired-mode, in
  Finder (OSX specific)."
  (interactive)
  (if (eq 'dired-mode major-mode)
      (shell-command "open .")
    (shell-command (concat "open -R '" (concat buffer-file-name "'")))))

;; goto next user buffer (no *Messages*, *eshell* etc.)
(defun next-user-buffer ()
  "Switch to next buffer in cyclic order. User buffers are those
  not starting with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name))
                (< i 50)) ; we need to have some maximum..
      (setq i (1+ i))
      (next-buffer))))

;; goto previous user buffer (no *Messages*, *eshell* etc.)
(defun previous-user-buffer ()
  "Switch to previous buffer in cyclic order. User buffers are
  those not starting with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name))
                (< i 50)) ; we need to have some maximum..
      (setq i (1+ i))
      (previous-buffer))))

;; Reload the conf-file.
(defun reload-conf ()
  "Reloads ~/.emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Opens (finds) ~/.emacs.d/init.el in current buffer.
(defun open-conf () 
  "Opens ~/.emacs.d/init.el for editing"
  (interactive)
  (find-file-existing "~/.emacs.d/init.el"))

;; Scroll one line up / down
(defun scroll-down-one-line ()
  "Scrolls down one line"
  (interactive)
  (scroll-down 2))

(defun scroll-up-one-line ()
  "Scrolls up one line"
  (interactive)
  (scroll-up 2))

;; Set exec path to be the same as the one from the shell
(defun set-exec-path-from-shell-path()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
  This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-path)

(defun string-starts-with (string prefix)
  "Returns non-nil if string STRING starts with PREFIX, otherwise nil."
  (and (>= (length string) (length prefix))
       (string-equal (substring string 0 (length prefix)) prefix)))

;;;;;;;;; WARNINGS

;; Ignore the load-path warning because I want my .emacs.d as I have
;; it now!
(defadvice display-warning
    (around no-warn-.emacs.d-in-load-path (type message &rest unused) activate)
  "Ignore the warning about the `.emacs.d' directory being in `load-path'."
  (unless (and (eq type 'initialization)
               (string-starts-with message "Your `load-path' seems to contain\nyour `.emacs.d' directory"))
    ad-do-it))


(provide 'init-functions)
