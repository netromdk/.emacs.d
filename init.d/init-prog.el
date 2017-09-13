(require 'req-package)

;; General compilation settings.
(setq compilation-window-height 30
      compilation-scroll-output 'first-error ; Scroll but stop at first error.
      compilation-skip-threshold 2           ; Skip anything less than errors.
      compilation-always-kill t)             ; Don't ask, just start new compilation.

;; Turn off adaptive process buffering when using compilation mode because it speeds up immensely
;; when there is a lot of output in the buffer.
(add-hook 'compilation-mode-hook
          (lambda () (setq process-adaptive-read-buffering nil)))

;; Turn it back on again when finished.
(add-hook 'compilation-finish-functions
          (lambda (buffer string)
            (setq process-adaptive-read-buffering t)))

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

;; Use C-cC-c to recompile and C-cC-f to goto next error.
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Using local-set-key because defining the bindings in prog-mode-map will get
            ;; overridden by c++-mode bindings, for instance. This shadows them instead.
            (local-set-key "\C-c\C-c" 'recompile)
            (local-set-key "\C-c\C-f" 'next-error)))

;; Closes *compilation* buffer after successful compilation, and otherwise when the failure was
;; fixed to compile, it restores the original window configuration.
(req-package bury-successful-compilation
  :config
  (add-hook 'prog-mode-hook 'bury-successful-compilation))

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
  (setq highlight-thing-delay-seconds 1.5)
  (setq highlight-thing-limit-to-defun t) ;; Limit to current function.
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-case-sensitive-p t)
  (add-hook 'prog-mode-hook 'highlight-thing-mode))

;; Avoid escape nightmares by editing string in separate buffer.
(req-package string-edit
  :bind ("C-c e" . string-edit-at-point))

;; Visualize certain like space at end of line and trailing characters after
;; fill column.
(setq whitespace-style '(face empty tabs lines-tail trailing tab-mark))
(setq whitespace-line-column global-fill-column)

(add-hook 'prog-mode-hook 'whitespace-mode)

;; Formatting code via clang-format-region.
(req-package clang-format)

;; Highlights escape sequences like \t, \n etc. in strings in programming modes.
(req-package highlight-escape-sequences
  :config
  ;; Has its own `hes-mode-alist' that specifies which modes it supports.
  (hes-mode))

;; ;; Annotate depth when it gets too deep.
;; (req-package annotate-depth
;;   :config
;;   (add-hook 'prog-mode-hook 'annotate-depth-mode)
;;   (add-hook 'annotate-depth-mode-hook
;;             (lambda ()
;;               (if (equal major-mode 'emacs-lisp-mode)
;;                   (setq-local annotate-depth-threshold 10)
;;                 (when (equal major-mode 'c++-mode)
;;                   (setq-local annotate-depth-threshold 5))))))

;; C/C++

(req-package modern-cpp-font-lock
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(setq use-qt-tab-width nil)
(defun turn-on-qt-tab-width ()
  "Use Qt tab width (4 spaces)."
  (interactive)
  (setq use-qt-tab-width t)
  (revert-buffer :ignore-auto :noconfirm))
(defun turn-off-qt-tab-width ()
  "Use general tab width."
  (interactive)
  (setq use-qt-tab-width nil)
  (revert-buffer :ignore-auto :noconfirm))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; If in Qt code then use 4 spaces as tabs, otherwise the general tab width.
            (if (bound-and-true-p use-qt-tab-width)
                (setq tab-width 4)
              (setq tab-width general-tab-width))
            (setq c-basic-offset tab-width)
            (setq indent-tabs-mode nil)

            ;; Run clang-format on region or buffer.
            (local-set-key (kbd "C-c f") 'clang-format-region-or-buffer)))

(req-package cc-mode
  :require key-chord
  :config
  (defun my-colon-at-the-end ()
    (interactive)
    (progn
      (move-end-of-line nil)
      (cycle-spacing 0)                 ; Remove all spaces.
      (insert ";")))
  (key-chord-define c-mode-base-map ";;" 'my-colon-at-the-end))

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

;; Swift
(req-package swift-mode
  :config
  (setq swift-indent-offset general-tab-width))

;; Elisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode)
            (local-set-key (kbd "C-c b") 'eval-buffer)
            (local-set-key (kbd "C-c r") 'eval-region)))

;; Shell script
(add-hook 'sh-mode (lambda () (setq-local sh-indentation general-tab-width)))

(setq auto-mode-alist
      (append '(("\\.[z]?sh$"  . sh-mode))
              auto-mode-alist))

;; JavaScript
(add-hook 'js-mode (lambda () (setq-local js-indent-level general-tab-width)))

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

;; C#
(req-package csharp-mode
  :mode (("\\.cs$" . csharp-mode))

  :config
  (add-hook 'csharp-mode-hook
            (lambda ()
              (setq c-basic-offset general-tab-width))))

;; Markdown
(req-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))

  :config
  ;; Turn off auto-fill-mode beacuse markdown is sensitive about newlines.
  (add-hook 'markdown-mode-hook
            (lambda ()
              (auto-fill-mode 0)
              (visual-line-mode t))))

;; Marks TODO, FIXME etc. clearly.
(req-package fic-mode
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(req-package yasnippet
  :require helm
  :config
  ;; Add local snippets to override some of the defaults in elpa folder.
  (add-to-list 'yas-snippet-dirs yas-dir)

  ;; (setq yas-prompt-functions
  ;;       '(yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-x-prompt yas-no-prompt))

  (defun shk-yas/helm-prompt (prompt choices &optional display-fn)
    "Use helm to select a snippet. Put this into `yas/prompt-functions.'"
    (interactive)
    (setq display-fn (or display-fn 'identity))
    (if (require 'helm-config)
        (let (tmpsource cands result rmap)
          (setq cands (mapcar (lambda (x) (funcall display-fn x)) choices))
          (setq rmap (mapcar (lambda (x) (cons (funcall display-fn x) x)) choices))
          (setq tmpsource
                (list
                 (cons 'name prompt)
                 (cons 'candidates cands)
                 '(action . (("Expand" . (lambda (selection) selection))))
                 ))
          (setq result (helm-other-buffer '(tmpsource) "*helm-select-yasnippet"))
          (if (null result)
              (signal 'quit "user quit!")
            (cdr (assoc result rmap))))
      nil))
  (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)

  (yas-global-mode 1)

  ;; Disable normal tab expansion because it often interferes with
  ;; indentation.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-M-y") 'yas-expand)

  ;; Test if thing at cursor can expand with yas, if it can then change the color of the cursor.
  (setq default-cursor-color (face-background 'cursor))
  (setq yasnippet-can-fire-cursor-color "#4CB5F5")

  (defun yasnippet-can-fire-p (&optional field)
    (interactive)
    (setq yas--condition-cache-timestamp (current-time))
    (let (templates-and-pos)
      (unless (and yas-expand-only-for-last-commands
                   (not (member last-command yas-expand-only-for-last-commands)))
        (setq templates-and-pos (if field
                                    (save-restriction
                                      (narrow-to-region (yas--field-start field)
                                                        (yas--field-end field))
                                      (yas--templates-for-key-at-point))
                                  (yas--templates-for-key-at-point))))
      (and templates-and-pos (first templates-and-pos))))

  (defun my/change-cursor-color-when-can-expand (&optional field)
    (interactive)
    (when (eq last-command 'self-insert-command)
      (if (my/can-expand)
          (progn
            (set-cursor-color yasnippet-can-fire-cursor-color)
            ;; Change back 5 seconds afterwards to avoid border cases where it stays in the "can
            ;; expand" color when actually it can't.
            (run-at-time "5 sec" nil
                         (lambda ()
                           (set-cursor-color default-cursor-color))))
        (set-cursor-color default-cursor-color))))

  (defun my/can-expand ()
    "Return true if right after an expandable thing."
    (or (abbrev--before-point) (yasnippet-can-fire-p)))

  (add-hook 'post-command-hook 'my/change-cursor-color-when-can-expand))

(req-package helm-c-yasnippet
  :require yasnippet
  :bind ("C-c y" . helm-yas-complete))

(req-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)

  ;; Bindings
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key smartparens-mode-map (kbd "C-M-S-k") 'sp-kill-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-M-S-t") 'sp-transpose-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-M-S-s") 'sp-slurp-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-M-S-p") 'sp-push-hybrid-sexp)

  ;; General prog mode handling of "{}" to indent after hitting RET.
  (sp-with-modes '(c-mode c++-mode php-mode java-mode js-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))

  ;; Markdown modes
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "**" "**")
    (sp-local-pair "_" "_"))

  ;; Lisp modes
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil :wrap "C-(")
    (sp-local-pair "`" "'" :when '(sp-in-string-p)))

  (bind-key ";" 'sp-comment emacs-lisp-mode-map)

  ;; LaTeX modes
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "`" "'")
    (sp-local-tag "\"" "``" "''" :actions '(wrap))))

;; NSIS installer script file.
(req-package conf-mode
  :mode (("\\.nsi\\'" . conf-windows-mode)
         ("\\.nsis\\'" . conf-windows-mode)))

;; Show vertical lines to guide indentation.
(req-package indent-guide
  :config
  (setq indent-guide-char "|"
        indent-guide-delay 0.5)
  (add-hook 'prog-mode-hook 'indent-guide-mode))

;; Jump to definition for multiple languages without configuration.
(req-package dumb-jump
  :require (helm hydra)
  :config
  (setq dumb-jump-selector 'helm
        dumb-jump-max-find-time 5
        dumb-jump-aggressive nil)
  (defhydra dumb-jump-hydra (:columns 3 :idle 1)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))
  (global-set-key (kbd "M-g d") 'dumb-jump-hydra/body))


(provide 'init-prog)
