(require 'req-package)

;; on-screen mode to track reading markers.
(req-package on-screen
  :config
  (setq on-screen-highlight-method (quote fringe))
  (global-on-screen-mode 1))

(req-package window-numbering)

;; (req-package spaceline
;;   :require window-numbering
;;   :config
;;   (window-numbering-mode t)

;;   (require 'spaceline-config)
;;   (spaceline-spacemacs-theme)

;;   ;; Don't show unicode window numbers because they are too small to be seen
;;   ;; fast and clearly.
;;   (setq spaceline-window-numbers-unicode nil)

;;   (setq spaceline-minor-modes-separator " ")

;;   (spaceline-helm-mode)

;;   (spaceline-toggle-process-on)
;;   (spaceline-toggle-selection-info-on)
;;   (spaceline-toggle-hud-off))

;; Once for every computer, the `all-the-icons-install-fonts` function must be run to install fonts
;; needed by the modeline.
(req-package doom-modeline
  :require window-numbering
  :hook (after-init . doom-modeline-mode)
  :config
  (window-numbering-mode t)

  (setq doom-modeline-minor-modes nil
        doom-modeline-enable-word-count t
        doom-modeline-checker-simple-format t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-env-python-executable "python3"))

;; Remove or rename mode line values.
(req-package diminish
  :config
  (eval-after-load "anzu"
    '(diminish 'anzu-mode))

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


(provide 'init-ui)
