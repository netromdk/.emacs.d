(require 'req-package)

(req-package dired
  :require (ls-lisp key-chord)
  :config

  ;; Show human-readable sizes and show folders with "/" at the end.
  (setq dired-listing-switches "-lhaF")

  ;; In dired mode, use Emacs's emulation of "ls" because the system one most
  ;; often doesn't support the "--dired" argument.
  (setq ls-lisp-use-insert-directory-program nil)

  ;; Show directories first.
  (setq ls-lisp-dirs-first t)

  ;; Don't show link count but uid+gid.
  (setq ls-lisp-verbosity '(uid gid))

  (key-chord-define dired-mode-map "qq" 'dired-up-directory)

  ;; Unwanted files to be flagged for deletion on dired-flag-garbage-files or "%&".
  (setq dired-garbage-files-regexp
        "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|elc\\)\\)\\'")

  (define-key dired-mode-map "k" 'dired-do-delete)
  (define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)
  (define-key dired-mode-map "F" 'find-name-dired)

  ;; Enter editable (wdired) mode where file and folder names can be changed directly as a buffer.
  (define-key dired-mode-map "E" 'wdired-change-to-wdired-mode)

  ;; C-a goes to start of file entry (column 2).
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

  ;; M-< goes to first file, the fourth line unless in omit mode then second line.
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (if (bound-and-true-p dired-omit-mode)
        (dired-next-line 2)
      (dired-next-line 4)))
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
  (define-key dired-mode-map (kbd "M-p") 'dired-back-to-top)

  ;; M-> goes to last file.
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
  (define-key dired-mode-map (kbd "M-n") 'dired-jump-to-bottom))

;; dired-k makes directory listings more readable, adding a bit of color and some git status
;; information on files and directories.
(req-package dired-k
  :require dired
  :config
  ;; Run dired-k when dired buffer is opened.
  (add-hook 'dired-initial-position-hook 'dired-k)

  (define-key dired-mode-map "K" 'dired-k))

(req-package dired-sort
  :require dired
  :config
  (key-chord-define dired-mode-map "ss" 'dired-sort-size)
  (key-chord-define dired-mode-map "se" 'dired-sort-extension)
  (key-chord-define dired-mode-map "sc" 'dired-sort-ctime)
  (key-chord-define dired-mode-map "su" 'dired-sort-utime)
  (key-chord-define dired-mode-map "st" 'dired-sort-time)
  (key-chord-define dired-mode-map "sn" 'dired-sort-name))


(provide 'init-dired)
