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

(defun msk/compilation-toggle-scroll ()
  "Toggle between not scrolling and scrolling until first error
in compilation mode."
  (interactive)
  (if (not compilation-scroll-output)
      (setq compilation-scroll-output 'first-error)
    (setq compilation-scroll-output nil)))

(defun msk/compilation-scroll-output-string ()
  (interactive)
  (if (not compilation-scroll-output)
      "No scroll"
    "Scroll until match"))

(defun msk/compilation-skip-threshold-string ()
  (interactive)
  (cond
   ((= compilation-skip-threshold 2)
    "Skip anything less than error")
   ((= compilation-skip-threshold 1)
    "Skip anything less than warning")
   ((<= compilation-skip-threshold 0)
    "Don't skip anything")))

(defun msk/compilation-toggle-threshold ()
  (interactive)
  (progn
    (setq compilation-skip-threshold (- compilation-skip-threshold 1))
    (when (< compilation-skip-threshold 0)
      (setq compilation-skip-threshold 2))))

(defun msk/compilation-command-string ()
  (interactive)
  (if (not compile-command)
      "None"
    compile-command))

(defun msk/compilation-last-error ()
  (interactive)
  (condition-case err
      (while t
        (next-error))
    (user-error nil)))

(req-package hydra
  :require helm
  :config
  ;; Easier cycling of yanking.
  (defhydra yank-pop-hydra ()
    "yank"
    ("C-y" yank nil)
    ("M-y" yank-pop nil)
    ("y" (yank-pop 1) "next")
    ("Y" (yank-pop -1) "prev")
    ("l" helm-show-kill-ring "list" :color blue))

  (global-set-key (kbd "M-y") #'yank-pop-hydra/yank-pop)
  (global-set-key (kbd "C-y") #'yank-pop-hydra/yank)

  (defhydra compilation-hydra (:columns 4)
    "
Command: %(msk/compilation-command-string)
%(msk/compilation-scroll-output-string) + %(msk/compilation-skip-threshold-string)
"
    ("c" compile "Compile")
    ("C" compile-from-buffer-folder "Compile from buffer folder")
    ("r" recompile "Recompile")
    ("k" kill-compilation "Stop")
    ("n" next-error "Next error")
    ("N" next-error-skip-warnings "Next error, skip warnings")
    ("p" previous-error "Previous error")
    ("f" first-error "First error")
    ("l" msk/compilation-last-error "Last error")
    ("s" msk/compilation-toggle-scroll "Toggle scroll")
    ("t" msk/compilation-toggle-threshold "Toggle threshold")
    ("q" nil "Cancel" :color blue))

  (global-set-key [(f5)] 'compilation-hydra/body)

  ;; Define hydra for programming modes.
  (add-hook 'prog-mode-hook
            (lambda ()
              ;; Using local-set-key because defining the bindings in prog-mode-map will get
              ;; overridden by c++-mode bindings, for instance. This shadows them instead.
              (local-set-key (kbd "C-c C-c") 'compilation-hydra/body))))

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
  :require helm
  :config
  (setq dumb-jump-selector 'helm
        dumb-jump-max-find-time 5
        dumb-jump-aggressive nil)
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

;; Haskell related packages

;; Setup: cabal install hasktags stylish-haskell hoogle
;; It is assumed that PATH is updated to point to where to find Haskell/cabal binaries!
(req-package haskell-mode
  :config
  (setq haskell-tags-on-save t
        haskell-process-type 'auto
        haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t
        haskell-process-log t)

  (defun my-haskell-hoogle-browser ()
    "Force hoogle to search via the browser for `haskell-ident-at-point'."
    (interactive)
    (let (command haskell-hoogle-command)
      (setq haskell-hoogle-command nil) ; `nil' forces usage of browser.
      (haskell-hoogle (haskell-ident-at-point))
      (setq haskell-hoogle-command command)))

  ;; TODO: Make hydra
  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c f") 'haskell-mode-stylish-buffer)
  (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile)
  (define-key haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)
  (define-key haskell-mode-map (kbd "C-c C-b") 'my-haskell-hoogle-browser)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)

  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)

  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

(req-package company-ghc
  :require company haskell-mode
  :config
  (setq company-ghc-show-info t)
  (add-hook 'haskell-mode-hook 'company-mode)
  (add-to-list 'company-backends 'company-ghc))

;; Setup: cabal install hindent
(req-package hindent
  :require haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

;; End of Haskell related packages

;; Language Server Protocol

(req-package lsp-mode
  :require hydra
  :config
  (defhydra msk/lsp-hydra (:color blue :hint nil)
    ;; Xref
    ("d" xref-find-definitions "Definitions" :column "Xref")
    ("D" xref-find-definitions-other-window "-> other win")
    ("r" xref-find-references "References")
    ("a" xref-find-apropos "Apropos")

    ;; Hierarchies
    ("m" cquery-member-hierarchy "Member" :column "Hierarchies")
    ("i" cquery-inheritance-hierarchy "Inheritance")
    ("c" cquery-call-hierarchy "Calls")

    ;; LSP
    ("R" lsp-rename "Rename" :column "LSP")
    ("t" lsp-goto-type-definition "Type definition")

    ;; Code Lens
    ("l" cquery-code-lens-mode "Toggle" :column "Code Lens")
    ("u" cquery-request-code-lens "Update")

    ("q" nil "Cancel" :column "Misc"))

  (add-hook 'lsp-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c C-l") 'msk/lsp-hydra/body))))

;; Install cquery server executable externally.
;;   Homebrew: brew install cquery
;; It tries to locate the "compile_commands.json" file in the root of the project or in root/build.
(req-package cquery
  :require lsp-mode
  :config
  ;;(setq cquery-extra-args '("--log-file=/tmp/cq.log"))

  (defun msk/cquery-enable ()
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil)))
  (add-hook 'c-mode-hook #'msk/cquery-enable)
  (add-hook 'c++-mode-hook #'msk/cquery-enable))

;; Xref

;; Don't show prompt unless nothing is under point or if it has to show it.
(setq xref-prompt-for-identifier nil)

;; Show xref results in helm.
(req-package helm-xref
  :require helm
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))


(provide 'init-prog)
