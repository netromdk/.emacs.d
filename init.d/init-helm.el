(require 'req-package)

(req-package helm
  :require recentf
  :config
  (progn
    (setq helm-candidate-number-limit 100)
    (setq helm-display-source-at-screen-top t)
    (setq helm-exit-idle-delay 0)
    (setq helm-full-frame nil)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-M-x-fuzzy-match t)
    (setq helm-recentf-fuzzy-match t)
    (setq helm-apropos-fuzzy-match t)
    (setq helm-ff-file-name-history-use-recentf t)
    (setq helm-split-window-default-side (quote below))
    (setq helm-reuse-last-window-split-state nil)
    (setq helm-split-window-in-side-p t) ;; split in same window
    (setq helm-quick-update t) ;; don't show invisible candidates

    ;; Resize helm according to number of results within min/max height.
    (setq helm-autoresize-min-height 10)
    (setq helm-autoresize-max-height 40)
    (helm-autoresize-mode t)

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)

    ;; Enhance the help menu using helm functionality.
    (define-key 'help-command (kbd "a") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)
    (define-key 'help-command (kbd "C-l") 'helm-locate-library)
    (define-key 'help-command (kbd "SPC") 'helm-all-mark-rings)

    ;; Redefine "C-x 2" (vertical split) and "C-x 3" (horizontal split) to be
    ;; more useful by switching to the new buffer and running helm-mini in it.
    (defun my/vsplit-helm (prefix)
      "Split the window vertically and display helm-mini in calling buffer."
      (interactive "p")
      (split-window-vertically)
      (helm-mini))

    (defun my/hsplit-helm (prefix)
      "Split the window horizontally and display helm-mini in calling buffer."
      (interactive "p")
      (split-window-horizontally)
      (helm-mini))

    (global-set-key (kbd "C-x 2") 'my/vsplit-helm)
    (global-set-key (kbd "C-x 3") 'my/hsplit-helm)))

(req-package helm-swoop
  :require helm
  :config
  (progn
    (setq helm-swoop-split-direction (quote split-window-vertically))
    (setq helm-swoop-split-with-multiple-windows t)

    ;; Activate helm-swoop on isearch results.
    (define-key isearch-mode-map
      (kbd "M-i") 'helm-swoop-from-isearch)
    (define-key isearch-mode-map
      (kbd "M-I") 'helm-multi-swoop-all-from-isearch)))

(req-package helm-gtags
  :require helm
  :config
  (progn
    (setq helm-gtags-maximum-candidates 1000)

    ;; Enable helm-gtags-mode
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    ;; Set key bindings
    (eval-after-load "helm-gtags"
      '(progn
         (define-key helm-gtags-mode-map
           (kbd "M-t") 'helm-gtags-find-tag)
         (define-key helm-gtags-mode-map
           (kbd "M-r") 'helm-gtags-find-rtag)
         (define-key helm-gtags-mode-map
           (kbd "M-s") 'helm-gtags-find-symbol)))))

(req-package helm-flx
  :require (helm flx)
  :config
  (progn
    ;; Use flx for better search results.
    (helm-flx-mode +1)))

(req-package helm-ag
  :require helm
  :config
  (progn
    (setq helm-ag-base-command "ag --nocolor --nogroup --smart-case --stats")))


(provide 'init-helm)
