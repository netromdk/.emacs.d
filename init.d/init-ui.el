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
    '(diminish 'flyspell-mode "ᚨ"))

  (eval-after-load "company"
    '(diminish 'company-mode "ç"))

  (eval-after-load "whitespace"
    '(diminish 'whitespace-mode "ω"))

  (eval-after-load "smartparens"
    '(diminish 'smartparens-mode))

  (eval-after-load "autorevert"
    (progn
      '(diminish 'auto-revert-mode "αr")
      '(diminish 'global-auto-revert-mode "αr")))

  (eval-after-load "org-table"
    '(diminish 'orgtbl-mode "ꙮτ"))

  (eval-after-load "org"
    '(diminish 'orgstruct-mode "ꙮσ"))

  (eval-after-load "rainbow-mode"
    '(diminish 'rainbow-mode))

  (eval-after-load "helm-gtags"
    '(diminish 'helm-gtags-mode "τ"))

  (eval-after-load "yasnippet"
    '(diminish 'yas-minor-mode "γ"))

  (eval-after-load "highlight-thing"
    '(diminish 'highlight-thing-mode))

  (eval-after-load "hi-lock"
    '(diminish 'hi-lock-mode)))

;; Line numbers. Is faster than the built-in linum mode.
(req-package nlinum
  :require linum magit
  :config
  ;; Precalculate the line number width to avoid horizontal jumps on scrolling.
  (add-hook 'nlinum-mode-hook
            (lambda ()
              (when nlinum-mode
                (setq nlinum--width
                      (length (number-to-string
                               (count-lines (point-min) (point-max)))))
                (nlinum--flush))))

  ;; Enable line numbers but disable for certain modes and minibuffer.
  (setq msk/nlinum-disabled-modes-list
        '(eshell-mode
          compilation-mode
          magit-status-mode
          magit-popup-mode))
  (define-globalized-minor-mode msk/global-nlinum-mode
    nlinum-mode
    (lambda ()
      (progn
        (unless (or (minibufferp)
                    (member major-mode msk/nlinum-disabled-modes-list))
          (nlinum-mode 1)))))

  (msk/global-nlinum-mode))

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


(provide 'init-ui)
