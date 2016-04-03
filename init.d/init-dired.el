(require 'req-package)

(req-package dired
  :require (ls-lisp key-chord)
  :config
  (setq dired-listing-switches "-lha") ;; Show human-readable sizes.

  ;; In dired mode, use Emacs's emulation of "ls" because the system one most
  ;; often doesn't support the "--dired" argument.
  (setq ls-lisp-use-insert-directory-program nil)

  ;; Automatically refresh dired buffer on changes.
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  (key-chord-define dired-mode-map "qq" 'dired-up-directory)

  ;; Unwanted files to be flagged for deletion on dired-flag-garbage-files or "%&".
  (setq dired-garbage-files-regexp
        "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|elc\\)\\)\\'")

  (define-key dired-mode-map (kbd "k") 'dired-do-delete)
  (define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)

  ;; C-a goes to start of file entry (column 2).
  (defun dired-back-to-start-of-files ()
    (interactive)
    (backward-char (- (current-column) 2)))
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

  ;; M-< goes to first file, the fourth line.
  (defun dired-back-to-top ()
    (interactive)
    (beginning-of-buffer)
    (dired-next-line 4))
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

  ;; M-> goes to last file.
  (defun dired-jump-to-bottom ()
    (interactive)
    (end-of-buffer)
    (dired-next-line -1))
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom))

;; dired-k makes directory listings more readable, adding a bit of color and some git status
;; information on files and directories.
(req-package dired-k
  :require dired
  :config
  ;; Run dired-k when dired buffer is opened.
  (add-hook 'dired-initial-position-hook 'dired-k)

  (define-key dired-mode-map "K" 'dired-k))


(provide 'init-dired)
