(defun byte-compile-confs ()
  "Byte-compiles all configuration files."
  (interactive)
  (byte-compile-file (concat general-file ".el") t)
  (byte-compile-file (concat functions-file ".el") t))

(defun byte-compile-confs-if-not-present ()
  "If the .elc files are not there then compile all configuration
files (I just assume all are not there if 'general.elc' is not
there). This is necessary because the auto-compile feature won't
compile to .elc if not already present first."
  (interactive)
  (if (not (file-exists-p (concat general-file ".elc")))
      (byte-compile-confs)))

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

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line. If
point is already there, move to the beginning of the
line. Effectively toggle between the first non-whitespace
character and the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun add-to-multiple-hooks (function hooks)
  "Run `function' for each hook in `hooks'."
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defun sudo-find-file (file)
  "Find file with sudo/tramp."
  (interactive
   (list
    (read-file-name "sudo find file: ")))
  (find-file (format "/sudo::%s" file)))

(defun sudo-find-current ()
  "Find current buffer file with sudo/tramp."
  (interactive)
  (sudo-find-file (buffer-file-name)))

(defun include-guard-header ()
  "Generates an include guard header symbol using the path to the
current file but only from the project root, which is determined
by looking for a '.git'. The use case is in a C++/C include
guard: #ifndef `(include-guard-header)`.."
  (let ((project-root (expand-file-name (locate-dominating-file (buffer-file-name) ".git"))))
    (replace-regexp-in-string "[\\.\\\\//]" "_"
                              (upcase (substring (buffer-file-name) (length project-root))))))

(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE."
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

(defun mkdir (dir)
  "Make directory `dir' and don't signal error if it already exists."
  (when (not (file-directory-p dir))
    (make-directory dir)))
