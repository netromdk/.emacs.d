(defun byte-compile-confs ()
  "Byte-compiles all configuration files."
  (interactive)
  (byte-compile-file (concat general-file ".el") t)
  (byte-compile-file (concat functions-file ".el") t)
  (byte-recompile-directory init-dir 0 t))

;; convert current buffer to unix EOLs
(defun to-unix-eol ()
  "Change current buffer's line ending to unix convention."
  (interactive)
  (progn
    (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos
    (save-buffer)))

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

(defun scroll-line-up ()
  "Scrolls down one line"
  (interactive)
  (scroll-down 1))

(defun scroll-line-down ()
  "Scrolls up one line"
  (interactive)
  (scroll-up 1))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun indent-buffer ()
  "Indents the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indents a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end)))
      (progn
        (indent-buffer)))))

(defun untabify-buffer ()
  "Untabifies the entire buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun untabify-region-or-buffer ()
  "Untabifies a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (untabify (region-beginning) (region-end)))
      (progn
        (untabify-buffer)))))

(defun cleanup-region-or-buffer ()
  "Perform cleanup operations on the whitespace content of a region or buffer."
  (interactive)
  (indent-region-or-buffer)
  (untabify-region-or-buffer)
  ;; Also works on region or buffer.
  (whitespace-cleanup))

(defun clang-format-region-or-buffer ()
  "Perform clang-format on region or buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (clang-format-region (region-beginning) (region-end))
      (clang-format-buffer))))

(defun cleanup-region-or-buffer-clang ()
  "Perform cleanup operations on the whitespace content of a region or buffer using clang-format."
  (interactive)
  (indent-region-or-buffer)
  (untabify-region-or-buffer)
  ;; Also works on region or buffer.
  (whitespace-cleanup)
  (clang-format-region-or-buffer))

;; Set exec path to be the same as the one from the shell
(defun set-exec-path-from-shell-path ()
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

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapcar 'kill-buffer (buffer-list))
  (delete-other-windows))

(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun indent-for-comment-and-indent ()
  "Indent for comment and indent again afterwards. This is to
ensure the indentation level. For instance, in C++ it will insert
comment to the far right of the line but if used to start a
comment on a line of its own then it will be to the far right,
and the extra indentation fixes that."
  (interactive)
  (indent-for-comment)
  (indent-for-tab-command))
