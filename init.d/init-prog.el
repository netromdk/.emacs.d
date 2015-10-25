(require 'req-package)

;; General compilation settings.
(setq compilation-scroll-output t)
(setq compilation-window-height 30)

(defun next-error-skip-warnings ()
  (interactive)
  (let (threshold compilation-skip-threshold)
    (setq compilation-skip-threshold 2)
    (next-error)
    (setq compilation-skip-threshold threshold)))

(global-set-key [(C-f5)] 'compile)
(global-set-key [(f5)] 'recompile)
(global-set-key [(f6)] 'next-error)
(global-set-key [(C-f6)] 'next-error-skip-warnings)

(req-package dash-at-point
  :config
  (progn
    (global-set-key "\C-cd" 'dash-at-point)

    (add-to-list 'dash-at-point-mode-alist '(c-mode . "c,manpages"))
    (add-to-list 'dash-at-point-mode-alist '(c++-mode . "cpp,qt,c,manpages,lux"))
    (add-to-list 'dash-at-point-mode-alist '(python-mode . "py,flask"))
    (add-to-list 'dash-at-point-mode-alist '(cmake-mode . "cmake"))
    (add-to-list 'dash-at-point-mode-alist '(js-mode . "js"))))

;; Highlights hexcolors, like #aabbcc and Red.
(req-package rainbow-mode
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-mode)))

;; For programming modes, show delimiters with variying colors to easily
;; distinguish between them.
(req-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

;; C/C++
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq tab-width 2)
            (setq c-basic-offset tab-width)
            (setq indent-tabs-mode nil)))

(setq auto-mode-alist
      (append '(("\\.c$"  . c-mode)
                ("\\.C$"  . c++-mode)
                ("\\.h$"  . c++-mode)
                ("\\.hpp$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.hh$" . c++-mode)
                ("\\.cc$" . c++-mode))
              auto-mode-alist))

;; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c b") 'eval-buffer)
            (local-set-key (kbd "C-c r") 'eval-region)))

;; Shell script
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)))

;; JavaScript
(req-package js
  :config
  (progn
    (add-hook 'js-mode-hook
              (lambda () (setq js-indent-level 2)))))

;; Objective-C
(setq auto-mode-alist
      (append '(("\\.mm$" . objc-mode))
              auto-mode-alist))

;; PHP
(req-package php-mode)

;; CSS
(setq auto-mode-alist
      (append '(("\\.css$" . css-mode)
                ("\\.style$" . css-mode))
              auto-mode-alist))


(provide 'init-prog)
