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
