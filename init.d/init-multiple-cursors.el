;; Multiple cursors
(require 'req-package)

(req-package multiple-cursors
  :require hydra
  :config

  (defhydra multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all dwim
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :color blue)
  ("a" mc/mark-all-like-this-dwim :color blue)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :color blue)
  ("q" nil))

  ;;(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c n") 'multiple-cursors-hydra/mc/mark-next-like-this)

  ;;(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c p") 'multiple-cursors-hydra/mc/mark-previous-like-this)

  (global-set-key (kbd "C-c a") 'mc/mark-all-like-this-dwim)
  (global-set-key (kbd "C-c m") 'mc/mark-all-dwim))


(provide 'init-multiple-cursors)
