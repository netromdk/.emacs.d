(require 'req-package)

;; on-screen mode to track reading markers.
(req-package on-screen
  :config
  (setq on-screen-highlight-method (quote fringe))
  (global-on-screen-mode 1))

(req-package window-numbering)

(req-package spaceline
  :require window-numbering
  :config
  (window-numbering-mode t)

  (require 'spaceline-config)
  (spaceline-spacemacs-theme)

  ;; Don't show unicode window numbers because they are too small to be seen
  ;; fast and clearly.
  (setq spaceline-window-numbers-unicode nil)

  (setq spaceline-minor-modes-separator " ")

  (spaceline-helm-mode)

  (spaceline-toggle-process-on)
  (spaceline-toggle-selection-info-on)
  (spaceline-toggle-hud-off))

;; Remove or rename mode line values.
(req-package diminish
  :config
  (eval-after-load "anzu"
    '(diminish 'anzu-mode))

  (eval-after-load "vim-empty-lines-mode"
    '(diminish 'vim-empty-lines-mode))

  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode))

  (eval-after-load "fic-mode"
    '(diminish 'fic-mode))

  (eval-after-load "flyspell"
    '(diminish 'flyspell-mode "fs"))

  (eval-after-load "company"
    '(diminish 'company-mode "c"))

  (eval-after-load "whitespace"
    '(diminish 'whitespace-mode "ω"))

  (eval-after-load "smartparens"
    '(diminish 'smartparens-mode))

  (eval-after-load "autorevert"
    (progn
      '(diminish 'auto-revert-mode "ar")
      '(diminish 'global-auto-revert-mode "ar")))

  (eval-after-load "rainbow-mode"
    '(diminish 'rainbow-mode))

  (eval-after-load "helm-gtags"
    '(diminish 'helm-gtags-mode))

  (eval-after-load "yasnippet"
    '(diminish 'yas-minor-mode "y"))

  (eval-after-load "highlight-thing"
    '(diminish 'highlight-thing-mode))

  (eval-after-load "hi-lock"
    '(diminish 'hi-lock-mode))

  (eval-after-load "irony"
    '(diminish 'irony-mode "ir"))

  (eval-after-load "eldoc"
    '(diminish 'eldoc-mode))

  ;; Diminish because the errors, warnings and info counts are shown in spaceline.
  (eval-after-load "flycheck"
    '(diminish 'flycheck-mode "fc"))

  (eval-after-load "ace-isearch"
    '(diminish 'ace-isearch-mode))

  (eval-after-load "auto-dim-other-buffers"
    '(diminish 'auto-dim-other-buffers-mode))

  (eval-after-load "indent-guide"
    '(diminish 'indent-guide-mode)))

;; Line numbers. Is faster than the built-in linum mode.
(req-package nlinum
  :require linum
  :config
  ;; Precalculate the line number width to avoid horizontal jumps on scrolling. Or it disables the
  ;; mode if there are too many lines (10000 currently) because it will make them look wrong, like
  ;; having multiple lines with the same line number.
  (add-hook 'nlinum-mode-hook
            (lambda ()
              (when nlinum-mode
                (let ((lines (count-lines (point-min) (point-max))))
                  (if (> lines 10000)
                      (progn
                        (message "Disabling nlinum-mode because there are too many lines.")
                        (nlinum-mode -1))
                    (progn
                      (setq nlinum--width (length (number-to-string lines)))
                      (nlinum--flush)))))))

  (add-to-multiple-hooks
   'nlinum-mode
   '(prog-mode-hook
     text-mode-hook)))

(req-package highlight-current-line
  :config
  (highlight-current-line-minor-mode)
  (highlight-current-line-on t))

;; Show vim-like ~ at the end of buffers to show end-of-file.
(req-package vim-empty-lines-mode
  :config
  (global-vim-empty-lines-mode))

;; Shows the number of matches for searches.
(req-package anzu
  :config
  (defalias 'qr 'anzu-query-replace)
  (defalias 'qrr 'anzu-query-replace-regexp)

  ;; Don't add to modeline because spaceline will show anzu.
  (setq anzu-cons-mode-line-p nil)

  ;; Deactivate region, if any, when using anzu replace functionality because it's
  ;; hard to see the search results with an active region as well.
  (setq anzu-deactivate-region t)

  ;; Change the mode-line text summary of search/replace results.
  (defun msk-anzu-update-func (here total)
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "%d/%d" here total))
                      (replace-query (format "%d replaces" total))
                      (replace (format "%d/%d" here total)))))
        (propertize status 'face 'anzu-mode-line))))
  (setq anzu-mode-line-update-function #'msk-anzu-update-func)

  (global-anzu-mode t))

;; Easy window resizing using keys.
(req-package windresize
  :config
  (defalias 'wr 'windresize))

;; Show one buffer and hiding all others, do again to restore buffers.
(req-package zygospore
  :config
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

;; Visually makes non-current buffers less prominent.
(req-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode t))

;; Dashboard shows recent files and projects in a startup buffer.
(req-package dashboard
  :config
  (setq dashboard-startup-banner 2 ; text "Emacs" banner
        dashboard-items '((recents  . 10)
                          (projects . 10)))
  (dashboard-setup-startup-hook))

(req-package tabbar
  :require projectile
  :config
  ;; Function that creates groups from buffer names/modes.
  (defun my-tabbar-group-for-buffer ()
    (list
     (cond
      ((or (get-buffer-process (current-buffer))
           ;; Check if the major mode derives from `comint-mode' or
           ;; `compilation-mode'.
           (tabbar-buffer-mode-derived-p
            major-mode '(comint-mode compilation-mode)))
       "Process")

      ((member (buffer-name)
               '("*dashboard*" "*scratch*" "*Messages*"))
       "Common")

      ((eq major-mode 'dired-mode)
       "Dired")

      ((memq major-mode
             '(help-mode apropos-mode Info-mode Man-mode))
       "Help")

      ((memq major-mode
             '(rmail-mode
               rmail-edit-mode vm-summary-mode vm-mode mail-mode
               mh-letter-mode mh-show-mode mh-folder-mode
               gnus-summary-mode message-mode gnus-group-mode
               gnus-article-mode score-mode gnus-browse-killed-mode))
       "Mail")

      ((memq major-mode
             '(compilation-mode))
       "Compilation")

      ;; Buffers I don't want in other groups because they are either irrelevant or doesn't show the
      ;; tabbar correctly at the top.
      ((or (memq major-mode
                 '(completion-list-mode magit-log-mode magit-diff-mode))
           (member (buffer-name)
                   '("*helm find files*" "*helm projectile*" "*helm mini*" "*Helm Swoop*"
                     "*helm M-x*" "*helm apropos*" "*clang-error*" "*clang-output*"))
           )
       "Unwanted Buffers")

      ;; Group buffers with file names related to unique projectile groups.
      ((and (projectile-project-p)
            (buffer-file-name))
       (format "Projectile: %s" (projectile-project-name)))

      (t
       ;; Return `mode-name' if not blank, `major-mode' otherwise.
       (if (and (stringp mode-name)
                ;; Take care of preserving the match-data because this
                ;; function is called when updating the header line.
                (save-match-data (string-match "[^ ]" mode-name)))
           mode-name
         (symbol-name major-mode))
       ))))

  (setq tabbar-use-images nil
        tabbar-background-color "#252525"
        tabbar-separator-value #(" " 0 1
                                 (display
                                  (space :width 0.5)
                                  pointer arrow face tabbar-separator))
        tabbar-buffer-groups-function 'my-tabbar-group-for-buffer)

  (define-key tabbar-mode-map (kbd "C-M-n") 'tabbar-forward-tab)
  (define-key tabbar-mode-map (kbd "C-M-p") 'tabbar-backward-tab)
  (define-key tabbar-mode-map (kbd "M-N") 'tabbar-forward-group)
  (define-key tabbar-mode-map (kbd "M-P") 'tabbar-backward-group)

  (tabbar-mode))


(provide 'init-ui)
