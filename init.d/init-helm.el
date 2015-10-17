(require 'req-package)

(req-package helm
  :config
  (progn
    (setq helm-candidate-number-limit 100)
    (setq helm-display-source-at-screen-top t)
    (setq helm-exit-idle-delay 0)
    (setq helm-full-frame nil)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-ff-file-name-history-use-recentf t)
    (setq helm-split-window-default-side (quote below))
    (setq helm-reuse-last-window-split-state nil)
    (setq helm-split-window-in-side-p t) ;; split in same window
    (setq helm-quick-update t) ;; don't show invisible candidates

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)

    ;; Enhance the help menu using helm functionality.
    (define-key 'help-command (kbd "a") 'helm-apropos)
    (define-key 'help-command (kbd "r") 'helm-info-emacs)
    (define-key 'help-command (kbd "C-l") 'helm-locate-library)
    (define-key 'help-command (kbd "SPC") 'helm-all-mark-rings)))

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
  :require (helm ag)
  :config
  (progn
    (setq helm-ag-base-command "ag --nocolor --nogroup --smart-case --stats")))


(provide 'init-helm)
