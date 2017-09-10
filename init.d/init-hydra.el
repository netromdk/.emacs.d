(require 'req-package)

;; Lots of inspiration found here: https://github.com/abo-abo/hydra/wiki/
(req-package hydra
  :require helm flycheck multiple-cursors
  :config

  ;; Easier cycling of yanking.
  (defhydra hydra-yank-pop ()
    "yank"
    ("C-y" yank nil)
    ("M-y" yank-pop nil)
    ("y" (yank-pop 1) "next")
    ("Y" (yank-pop -1) "prev")
    ("l" helm-show-kill-ring "list" :color blue))

  (global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
  (global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

  ;; Easy nagivation without holding down the control all the time.
  (defhydra hydra-move ()
    "move"
    ("f" forward-char)
    ("F" forward-word)
    ("n" next-line)
    ("N" forward-paragraph)

    ("b" backward-char)
    ("B" backward-word)
    ("p" previous-line)
    ("P" backward-paragraph)

    ("a" smarter-move-beginning-of-line)
    ("e" move-end-of-line)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("v" scroll-up-command)
    ("V" scroll-down-command)
    ("l" recenter-top-bottom))

  (global-set-key (kbd "C-f") #'hydra-move/forward-char)
  (global-set-key (kbd "M-f") #'hydra-move/forward-word)
  (global-set-key (kbd "C-n") #'hydra-move/next-line)
  (global-set-key (kbd "M-n") #'hydra-move/forward-paragraph)

  (global-set-key (kbd "C-b") #'hydra-move/backward-char)
  (global-set-key (kbd "M-b") #'hydra-move/backward-word)
  (global-set-key (kbd "C-p") #'hydra-move/previous-line)
  (global-set-key (kbd "M-p") #'hydra-move/backward-paragraph)

  (global-set-key (kbd "C-a") #'hydra-move/smarter-move-beginning-of-line)
  (global-set-key (kbd "C-e") #'hydra-move/move-end-of-line)
  (global-set-key (kbd "M-<") #'hydra-move/beginning-of-buffer)
  (global-set-key (kbd "M->") #'hydra-move/end-of-buffer)
  (global-set-key (kbd "C-v") #'hydra-move/scroll-up-command)
  (global-set-key (kbd "M-v") #'hydra-move/scroll-down-command)
  (global-set-key (kbd "C-l") #'hydra-move/recenter-top-bottom)

  ;; Easier usage of dired by using "." in dired buffer.
  (defhydra hydra-dired (:hint nil)
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

  (define-key dired-mode-map "." 'hydra-dired/body)

  ;; Navigate flycheck errors more easily.
  (defhydra hydra-flycheck
    (:pre  (flycheck-list-errors)
     :post (quit-windows-on "*Flycheck errors*")
     :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil)))

(provide 'init-hydra)
