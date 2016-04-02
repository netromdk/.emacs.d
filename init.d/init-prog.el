(require 'req-package)

;; General compilation settings.
(setq compilation-window-height 30
      compilation-scroll-output t
      compilation-always-kill t) ;; Don't ask, just start new compilation.

(defun next-error-skip-warnings ()
  (interactive)
  (let (threshold compilation-skip-threshold)
    (setq compilation-skip-threshold 2)
    (next-error)
    (setq compilation-skip-threshold threshold)))

(defun compile-from-buffer-folder (cmd)
  (interactive
   (list
    (read-shell-command "Compile command (pwd): ")))
  (compile
   (format "cd `dirname '%s'` && %s" (buffer-file-name) cmd)))

(global-set-key [(C-f5)] 'compile)
(global-set-key [(S-f5)] 'compile-from-buffer-folder)
(global-set-key [(f5)] 'recompile)
(global-set-key [(f6)] 'next-error)
(global-set-key [(C-f6)] 'next-error-skip-warnings)

(req-package dash-at-point
  :config
  (global-set-key "\C-cd" 'dash-at-point)
  (add-to-list 'dash-at-point-mode-alist '(c-mode . "c,manpages"))
  (add-to-list 'dash-at-point-mode-alist '(c++-mode . "cpp,qt,c,manpages,lux"))
  (add-to-list 'dash-at-point-mode-alist '(python-mode . "py,flask"))
  (add-to-list 'dash-at-point-mode-alist '(cmake-mode . "cmake"))
  (add-to-list 'dash-at-point-mode-alist '(js-mode . "js")))

;; Highlights hexcolors, like #aabbcc and Red.
(req-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; For programming modes, show delimiters with variying colors to easily
;; distinguish between them.
(req-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlights numbers with another color so they are easier to spot.
(req-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Highlights thing at point.
(req-package highlight-thing
  :config
  (setq highlight-thing-delay-seconds 0.8)
  (setq highlight-thing-limit-to-defun t) ;; Limit to current function.
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-case-sensitive-p t)
  (add-hook 'prog-mode-hook 'highlight-thing-mode))

;; Avoid escape nightmares by editing string in separate buffer.
(req-package string-edit
  :bind ("C-c e" . string-edit-at-point))

;; Formatting code via clang-format-region.
(req-package clang-format)

;; C/C++
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq tab-width 2)
            (setq c-basic-offset tab-width)
            (setq indent-tabs-mode nil)

            ;; Run clang-format on region or buffer.
            (local-set-key (kbd "C-c f") 'clang-format-region-or-buffer)))

(setq auto-mode-alist
      (append '(("\\.c$"  . c-mode)
                ("\\.C$"  . c++-mode)
                ("\\.h$"  . c++-mode)
                ("\\.hpp$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.hh$" . c++-mode)
                ("\\.cc$" . c++-mode))
              auto-mode-alist))

;; Detect if .h file is actually c-mode instead of c++-mode that I default them to be.
(req-package dummy-h-mode
  :commands dummy-h-mode
  :mode ("\\.h$" . dummy-h-mode)

  :init
  ;; Avoid error: "Variable binding depth exceeds max-specpdl-size".
  (setq max-specpdl-size 10000)

  :config
  (setq dummy-h-mode-default-major-mode 'c++-mode))

;; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c b") 'eval-buffer)
            (local-set-key (kbd "C-c r") 'eval-region)))

;; Shell script
(setq sh-indentation 2)

(setq auto-mode-alist
      (append '(("\\.sh$"  . sh-mode)
                ("\\.zsh$"  . sh-mode))
              auto-mode-alist))

;; JavaScript
(setq js-indent-level 2)

(req-package json-mode
  :mode ("\\.json$" . json-mode))

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
