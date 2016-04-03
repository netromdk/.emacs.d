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
    '(diminish 'abbrev-mode "Abv"))

  (eval-after-load "fic-mode"
    '(diminish 'fic-mode))

  (eval-after-load "company"
    '(diminish 'company-mode "Comp"))

  (eval-after-load "smartparens"
    '(diminish 'smartparens-mode))

  (eval-after-load "org-table"
    '(diminish 'orgtbl-mode "OrgT"))

  (eval-after-load "org"
    '(diminish 'orgstruct-mode "OrgS"))

  (eval-after-load "rainbow-mode"
    '(diminish 'rainbow-mode))

  (eval-after-load "helm-gtags"
    '(diminish 'helm-gtags-mode "HGt"))

  (eval-after-load "yasnippet"
    '(diminish 'yas-minor-mode))

  (eval-after-load "highlight-thing"
    '(diminish 'highlight-thing-mode))

  (eval-after-load "hi-lock"
    '(diminish 'hi-lock-mode)))

;; Line numbers.
(req-package linum
  :config
  ;; Show line number mode only while using goto-line.
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (let ((line-numbers-off-p (not linum-mode)))
      (unwind-protect
          (progn (when line-numbers-off-p
                   (linum-mode 1))
                 (call-interactively 'goto-line))
        (when line-numbers-off-p
          (linum-mode -1)))))

  ;; Since M-g is a prefix and hardly used it is mapped to
  ;; goto-line-with-feedback directly. If only wanting to remap the normal
  ;; binding then use [remap goto-line] instead of (kbd "M-g").
  (global-set-key (kbd "M-g") 'goto-line-with-feedback))

;; Disable right now because the line numbers are only shown when using M-g.
;; (req-package hlinum
;;   :config
;;   ;; highlights current line number in margin
;;   (hlinum-activate))

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

;; Use smartparens only to highlight matches of parens/blocks. No extra
;; insertion because it's annoying!
(req-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1))

;; Show one buffer and hiding all others, do again to restore buffers.
(req-package zygospore
  :config
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))


(provide 'init-ui)
