(require 'req-package)

(req-package dired
  :require (ls-lisp key-chord hydra)
  :config

  (defalias 'qrd 'find-name-dired)
  (defalias 'qrrd 'find-name-dired)

  ;; Show human-readable sizes and show folders with "/" at the end.
  (setq dired-listing-switches "-lhaF")

  ;; In dired mode, use Emacs's emulation of "ls" because the system one most
  ;; often doesn't support the "--dired" argument.
  (setq ls-lisp-use-insert-directory-program nil)

  ;; Show directories first.
  (setq ls-lisp-dirs-first t)

  ;; Don't show link count but uid+gid.
  (setq ls-lisp-verbosity '(uid gid))

  ;; Updated dired buffers on file system changes.
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  (key-chord-define dired-mode-map "qq" 'dired-up-directory)

  ;; Unwanted files to be flagged for deletion on dired-flag-garbage-files or "%&".
  (setq dired-garbage-files-regexp
        "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|elc\\)\\)\\'")

  (define-key dired-mode-map "k" 'dired-do-delete)
  (define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)

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
  (define-key dired-mode-map (kbd "M-n") 'dired-jump-to-bottom)

  ;; Easier usage of dired by using "." in dired buffer.
  (defhydra dired-hydra (:hint nil)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))

  (define-key dired-mode-map "." 'dired-hydra/body))

(req-package dired-sort
  :require dired
  :config
  (key-chord-define dired-mode-map "ss" 'dired-sort-size)
  (key-chord-define dired-mode-map "se" 'dired-sort-extension)
  (key-chord-define dired-mode-map "sc" 'dired-sort-ctime)
  (key-chord-define dired-mode-map "su" 'dired-sort-utime)
  (key-chord-define dired-mode-map "st" 'dired-sort-time)
  (key-chord-define dired-mode-map "sn" 'dired-sort-name))

(req-package dired-narrow
  :require dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))


(provide 'init-dired)
