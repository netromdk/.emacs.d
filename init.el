;;; Emacs Configurations                              -*- no-byte-compile: t -*-

(let ((min-version "28.1"))
  (if (version< emacs-version min-version)
      (error "Emacs v. %s+ is required for this configuration!" min-version)))

;; Constants.
(defconst --emacs-start-time (current-time))
(defconst --lisp-dir (concat user-emacs-directory "lisp/"))
(defconst --misc-dir (concat user-emacs-directory "misc/"))
(defconst --yas-dir (concat user-emacs-directory "snippets/"))
(defconst --themes-dir (concat user-emacs-directory "themes/"))
(defconst --user-cache-dir (concat user-emacs-directory "cache/"))
(defconst --auto-save-dir (concat user-emacs-directory "auto-save/"))

;; Set to `t' to enable config loading benchmarking and showing results when finished.
(defconst --do-init-benchmark nil)

(setq custom-theme-directory --themes-dir)

;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(mustang-netrom))
 '(custom-safe-themes
   '("cab92f6ec3ec5ea85aef8be0a6cfc20c2b1f16990ba3cd7e0d98ed312610b466" default)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set the random number seed from the system's entropy pool.
(random t)

;; Prefer newest version of a file. This is especially useful for compiled files.
(setq load-prefer-newer t)

;; Load general stuff that other init.el things might use. Loading without extension to
;; automatically load the newest version of a file: .el or .elc.
(mapc (lambda (file)
        (load (file-name-sans-extension file)))
      (directory-files --lisp-dir t "\\.el$"))

;; Create necessary directories if missing.
(mkdir --user-cache-dir)
(mkdir --auto-save-dir)

(setq --initial-done-time (current-time))

;; Speed up loading by removing handlers until finished. It contains a lot of regexps for matching
;; handlers to file names but it is not necessary while loading.
(setq file-name-handler-alist-old file-name-handler-alist
      file-name-handler-alist nil)

;; Speed up loading by disabling garbage collection until finished.
(setq gc-cons-threshold most-positive-fixnum)

;;;;; Setup and Bootstrap straight.el ;;;;;

(defvar --straight-bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (--straight-bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-repository-branch "master"
      straight-cache-autoloads t

      ;; Make `use-package` use `straight.el` by default so that `:straight t` is not necessary to
      ;; write for every `use-package` invocation.
      straight-use-package-by-default t

      straight-hosts '((github "github.com" ".git")
                       (gitlab "gitlab.com" ".git")
                       (bitbucket "bitbucket.com" ".git")
                       (codeberg "codeberg.org" ".git"))

      straight-profiles '((nil . "default.el")
                          ;; Packages which are pinned to a specific commit.
                          (pinned . "pinned.el")))

;; Load straight-x.el to make pinning of packages possible.
;; NOTE: Use `straight-x-pull-all' instead of `straight-pull-all', and use
;; `straight-x-freeze-versions' instead of `straight-freeze-versions'!
(load-library "straight-x")

;;;;; Pinned packages ;;;;;

;; Example:
;; (add-to-list 'straight-x-pinned-packages
;;              '("doom-modeline" . "156b02445c3360added80009ab3c1a33dd88c5d9")) ;; v3.3.1

;;;;; Timing ;;;;;

(setq --straight-init-done-time (current-time))

(defun show-elapsed-time (msg start end)
  (let ((elapsed (float-time (time-subtract end start))))
    (message "%s %.3fs" msg elapsed)))

(defun show-loading-info ()
  (let ((cur (current-time)))
    (message "============================")
    (show-elapsed-time "Initial setup:     " --emacs-start-time --initial-done-time)
    (show-elapsed-time "straight.el setup: " --initial-done-time --straight-init-done-time)
    (show-elapsed-time "Loaded packages:   " --straight-init-done-time cur)
    (show-elapsed-time "Total:             " --emacs-start-time cur)
    (message "============================")

    ;; Show message 2s later about total time so it's visible in the mini buffer as the last thing.
    (run-at-time "2 sec" nil
                 #'show-elapsed-time "Loaded config in" --emacs-start-time cur)))

;; Executed when loading is done.
(defun loading-done ()
  ;; Stop benchmarking if enabled and show tabulated findings.
  (when --do-init-benchmark
    (benchmark-init/deactivate)
    (run-at-time "2 sec" nil
                 #'benchmark-init/show-durations-tabulated))

  (show-loading-info)

  ;; Restore the file name handlers.
  (setq file-name-handler-alist file-name-handler-alist-old)

  ;; Garbage collect at every 20 MB allocated instead of the default 8 MB. This speeds up various
  ;; things.
  (setq gc-cons-threshold 20000000)

  ;; Start daemon server if not already running.
  (require 'server)
  (unless (server-running-p)
    (server-start))

  (byte-compile-confs))

;;;;; Package Configuration Macro ;;;;;

;; The `use-package` configuration macro is used throughout this configuration to isolate package
;; configurations in a tidy fashion.

;; Install `use-package` which `straight.el` will automatically modify to use itself instead of
;; `package.el` since `straight-use-package-by-default` is set above.
(straight-use-package 'use-package)

;; Verbosity when starting emacs with `--debug-init'.
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;;;;; Benchmarking ;;;;;

;; Load and activate config loading benchmarking. It is deactivated in `loading-done'.
(when --do-init-benchmark
  (use-package benchmark-init)
  (benchmark-init/activate))

;;;;; Mac Setup ;;;;;

(when (eq system-type 'darwin)
  (progn
    (if (or (eq window-system 'ns)
            (eq window-system 'mac))
        (progn
          ;; avoid, e.g., hiding with M-h etc. (Carbon Emacs specific)
          ;;(setq mac-pass-command-to-system nil)

          ;; Let command be meta and alt be alt.
          (setq mac-option-key-is-meta nil)
          (setq mac-command-key-is-meta t)
          (setq mac-command-modifier 'meta)
          (setq mac-option-modifier nil)

          ;; The font is located in the ./font/ folder. Install it on the system to be used.
          (set-frame-font "JetBrains Mono-12")))

    (defun remove-dos-eol ()
      "Do not show ^M in files containing mixed UNIX and DOS line endings."
      (interactive)
      (setq buffer-display-table (make-display-table))
      (aset buffer-display-table ?\^M []))

    ;; ..and run it on all files.
    (add-hook 'find-file-hook 'remove-dos-eol)

    (defun open-with-finder ()
      "Show current buffer-file, or directory if in Dired-mode, in Finder."
      (interactive)
      (if (eq 'dired-mode major-mode)
          (shell-command "open .")
        (shell-command (concat "open -R '" (concat buffer-file-name "'")))))

    (defun toggle-fullscreen ()
      "Toggle full screen."
      (interactive)
      (set-frame-parameter
       nil 'fullscreen
       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))))

;;;;; Linux / X11 Setup ;;;;;

(if (eq window-system 'x)
    (set-default-font "-misc-fixed-medium-r-normal--13-120-75-75-c-70-iso8859-15"))

;;;;; Windows ;;;;;

(if (eq window-system 'w32)
    ;; The font is located in the ./font/ folder. Install it on the system to be used.
    (set-frame-font "JetBrains Mono-9"))

;;;;; Shell ;;;;;

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;;; Bindings ;;;;;

(use-package beacon
  :config
  (setq beacon-blink-delay 0.1
        beacon-blink-duration 0.3
        beacon-color "#b1d631")

  (defun backward-paragraph-blink ()
    (interactive)
    (backward-paragraph)
    (beacon-blink))

  (defun forward-paragraph-blink ()
    (interactive)
    (forward-paragraph)
    (beacon-blink))

  (global-set-key (kbd "M-p") 'backward-paragraph-blink)
  (global-set-key (kbd "M-n") 'forward-paragraph-blink)

  (beacon-mode 1))

;; General bindings.
(global-set-key (kbd "C-c o")
                #'(lambda () (interactive) (ff-find-other-file nil t)))
(global-set-key (kbd "C-.") 'repeat)

;; Move to first whitespace or begninning of line if none. Pressing again goes to the beginning if
;; there was whitespace.
(global-set-key [remap move-beginning-of-line] 'smarter-move-beginning-of-line)

;; Cycle through "just one space", "no spaces" and original number of spaces,
;; instead of just "just one space". It does not delete newlines, too.
(global-set-key (kbd "M-SPC")
                #'(lambda () (interactive) (cycle-spacing +1)))

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; Indent, untabify and clean whitespace of region or buffer.
(global-set-key (kbd "C-c c") 'cleanup-region-or-buffer)

;; Intelligent line opening that also places cursor on new line.
(global-set-key (kbd "C-o") 'prelude-smart-open-line)
(global-set-key (kbd "M-o") 'open-line) ;; Default way.

(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.2)

  (key-chord-define-global "qq" 'kill-this-buffer)
  (key-chord-define-global "''" "`'\C-b")
  (key-chord-define-global ",," 'indent-for-comment-and-indent)
  (key-chord-define-global "((" 'kmacro-start-macro)

  (key-chord-mode 1))

;;;;; Window Movement ;;;;;

(use-package golden-ratio-scroll-screen
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

;;;;; Magit ;;;;;

(use-package magit
  :config
  ;; Bindings.
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; Show word-granularity differences within current diff hunk.
  (setq magit-diff-refine-hunk t)

  ;; Show status full screen.
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  ;; Cycle between how much info is shown in the margin: author + date, date, none.
  (defun magit-cycle-margin ()
    "Cycle visibility of the Magit margin.

,-> show with details --> show no details -- hide -.
`--------------------------------------------------'"
    (interactive)
    (if (not (magit-margin-option))
        (user-error "Magit margin isn't supported in this buffer")
      (pcase (list (nth 0 magit-buffer-margin)
                   (and (nth 3 magit-buffer-margin) t))
        (`(t t)
         (setf (nth 3 magit-buffer-margin) nil)
         (magit-set-buffer-margin nil t))
        (`(t nil)
         (setf (nth 0 magit-buffer-margin) nil)
         (magit-set-buffer-margin))
        (`(nil ,_)
         (setf (nth 0 magit-buffer-margin) t)
         (setf (nth 3 magit-buffer-margin) t)
         (magit-set-buffer-margin nil t)))))

  ;; Call `magit-staging' to show mode with only unstaged and staged changes diff where one can
  ;; stage/unstage chunks like normal.
  (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
    "Mode for showing staged and unstaged changes."
    :group 'magit-status)

  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))

  (defun magit-staging ()
    (interactive)
    (window-configuration-to-register :magit-fullscreen)
    (magit-mode-setup #'magit-staging-mode)
    (delete-other-windows))

  (defun magit-log-origin ()
    "Show log of origin of current branch."
    (interactive)
    (magit-log-other `(,(concat "origin/" (magit-get-current-branch))))))

;;;;; Selectrum ;;;;;

(use-package orderless
  :config
  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  ;; Use orderless for all completion, defaulting to regexp style.
  ;; If pattern ends with `~' then use flex style.
  ;; If pattern starts with `!' then exclude results using literal style.
  (setq completion-styles '(orderless)
        orderless-matching-styles '(orderless-regexp)
        orderless-style-dispatchers '(flex-if-twiddle without-if-bang)))

(use-package prescient
  :config
  (setq prescient-save-file (concat user-emacs-directory "prescient-save"))
  (prescient-persist-mode +1))

(use-package marginalia
  :config
  (marginalia-mode +1))

(use-package selectrum
  :requires orderless
  :config
  (setq selectrum-refine-candidates-function #'orderless-filter
        selectrum-highlight-candidates-function #'orderless-highlight-matches
        selectrum-count-style 'current/matches
        selectrum-max-window-height 15)
  (selectrum-mode +1))

(use-package selectrum-prescient
  :requires selectrum prescient
  :config
  ;; Use filtring from only `completion-styles' and not selectrum.
  (setq selectrum-prescient-enable-filtering nil)

  ;; But enable frequency and recency ordering from selectrum.
  (selectrum-prescient-mode +1))

;;;;; Hydra ;;;;;

(use-package hydra
  :config
  ;; Easier cycling of yanking.
  (defhydra yank-pop-hydra ()
    "yank"
    ("C-y" yank nil)
    ("M-y" yank-pop nil)
    ("y" (yank-pop 1) "next")
    ("Y" (yank-pop -1) "prev"))

  (global-set-key (kbd "M-y") #'yank-pop-hydra/yank-pop)
  (global-set-key (kbd "C-y") #'yank-pop-hydra/yank)

  (defhydra compilation-hydra (:columns 4)
    "
Command: %(netrom/compilation-command-string)
%(netrom/compilation-scroll-output-string) + %(netrom/compilation-skip-threshold-string)
"
    ("c" compile "Compile")
    ("C" compile-from-buffer-folder "Compile from buffer folder")
    ("r" recompile "Recompile")
    ("k" netrom/kill-compilation "Stop")
    ("n" next-error "Next error")
    ("N" next-error-skip-warnings "Next error, skip warnings")
    ("p" previous-error "Previous error")
    ("f" first-error "First error")
    ("l" netrom/compilation-last-error "Last error")
    ("s" netrom/compilation-toggle-scroll "Toggle scroll")
    ("t" netrom/compilation-toggle-threshold "Toggle threshold")
    ("q" nil "Cancel" :color blue))

  (global-set-key [(f5)] 'compilation-hydra/body)

  ;; Define hydra for programming modes.
  (add-hook 'prog-mode-hook
            (lambda ()
              ;; Using local-set-key because defining the bindings in prog-mode-map will get
              ;; overridden by c++-mode bindings, for instance. This shadows them instead.
              (when (member major-mode '(c++-mode c-mode))
                (local-set-key (kbd "C-c C-c") 'compilation-hydra/body)))))

;;;;; Help & Documentation ;;;;;

;; Helpful is an alternative to the built-in Emacs help that provides much more contextual
;; information.
(use-package helpful
  :config
  ;; Note that the built-in `describe-function' includes both functions and
  ;; macros. `helpful-function' is functions only, so we provide `helpful-callable' as a drop-in
  ;; replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)

  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)

  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command))

;;;;; Completion ;;;;;

;; Using hippie-expand instead of dabbrev-expand.
(global-set-key (kbd "M-/") 'hippie-expand)

;; Put dabbrev expansions first because it's most often what's expected.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-expand-whole-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-list
        try-expand-line))

;; Enable the lisp expansion functions for lisp modes.
(add-to-multiple-hooks
 (lambda ()
   (setq-local hippie-expand-try-functions-list
               '(try-expand-dabbrev
                 try-expand-dabbrev-all-buffers
                 try-expand-dabbrev-from-kill
                 try-expand-all-abbrevs
                 try-expand-whole-kill
                 try-complete-lisp-symbol-partially
                 try-complete-lisp-symbol
                 try-complete-file-name-partially
                 try-complete-file-name
                 try-expand-list
                 try-expand-line)))
 '(emacs-lisp-mode-hook
   lisp-interaction-mode-hook))

(use-package flx-ido
  :requires flx
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)

  ;; Disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package company
  :config
  (setq company-idle-delay 0.3)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

;; Show icons in company completion UI.
(use-package company-box
  :config
  (if (display-graphic-p)
      ;; Show font icons in windowed mode.
      (setq company-box-color-icon t)
    ;; Show compatible icons in terminal.
    (setq company-box-icons-alist 'company-box-icons-icons-in-terminal))
  :hook (company-mode . company-box-mode))

(use-package company-flx
  :requires company
  :config
  (company-flx-mode +1))

;;;;; Development ;;;;;

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)))

;; General compilation settings.
(setq compilation-window-height 30
      compilation-scroll-output 'first-error ; Scroll but stop at first error.
      compilation-skip-threshold 2           ; Skip anything less than errors.
      compilation-always-kill t)             ; Don't ask, just start new compilation.

;; Turn off adaptive process buffering when using compilation mode because it speeds up immensely
;; when there is a lot of output in the buffer. And tweak split-height-threshold so that it shows
;; the compilation buffer in the bottom because that looks best, otherwise it will show to the side
;; and replace any buffer already at that place whereas this solution keeps the buffers but shows it
;; at the bottom.
(setq old-split-height-threshold nil)
(add-hook 'compilation-mode-hook
          (lambda ()
            (setq old-split-height-threshold split-height-threshold
                  split-height-threshold --global-fill-column)
                  process-adaptive-read-buffering nil))

;; Turn it back on again when finished.
(add-hook 'compilation-finish-functions
          (lambda (buffer string)
            (setq split-height-threshold old-split-height-threshold
                  old-split-height-threshold nil
                  process-adaptive-read-buffering t)))

(defun next-error-skip-warnings ()
  (interactive)
  (let ((compilation-skip-threshold 2))
    (next-error)))

(defun compile-from-buffer-folder (cmd)
  (interactive
   (list
    (read-shell-command "Compile command (pwd): ")))
  (compile
   (format "cd `dirname '%s'` && %s" (buffer-file-name) cmd)))

(defun netrom/compilation-toggle-scroll ()
  "Toggle between not scrolling and scrolling until first error
in compilation mode."
  (interactive)
  (if (not compilation-scroll-output)
      (setq compilation-scroll-output 'first-error)
    (setq compilation-scroll-output nil)))

(defun netrom/compilation-scroll-output-string ()
  (interactive)
  (if (not compilation-scroll-output)
      "No scroll"
    "Scroll until match"))

(defun netrom/compilation-skip-threshold-string ()
  (interactive)
  (cond
   ((= compilation-skip-threshold 2)
    "Skip anything less than error")
   ((= compilation-skip-threshold 1)
    "Skip anything less than warning")
   ((<= compilation-skip-threshold 0)
    "Don't skip anything")))

(defun netrom/compilation-toggle-threshold ()
  (interactive)
  (progn
    (setq compilation-skip-threshold (- compilation-skip-threshold 1))
    (when (< compilation-skip-threshold 0)
      (setq compilation-skip-threshold 2))))

(defun netrom/compilation-command-string ()
  (interactive)
  (if (not compile-command)
      "None"
    compile-command))

(defun netrom/compilation-last-error ()
  (interactive)
  (condition-case err
      (while t
        (next-error))
    (user-error nil)))

(defun netrom/kill-compilation ()
  "Sometimes `kill-compilation' doesn't work because it finds the
wrong buffer. Here `compilation-find-buffer' uses non-nil
`AVOID-CURRENT' to only use current as a last resort."
  (interactive)
  (interrupt-process (get-buffer-process (compilation-find-buffer t))))

;; Closes *compilation* buffer after successful compilation, and otherwise when the failure was
;; fixed to compile, it restores the original window configuration.
(use-package bury-successful-compilation
  :config
  (add-hook 'prog-mode-hook 'bury-successful-compilation))

(use-package dash-at-point
  :config
  (global-set-key "\C-cd" 'dash-at-point)
  (add-to-list 'dash-at-point-mode-alist '(c-mode . "c,manpages"))
  (add-to-list 'dash-at-point-mode-alist '(c++-mode . "cpp,qt,c,manpages,lux"))
  (add-to-list 'dash-at-point-mode-alist '(python-mode . "py,flask"))
  (add-to-list 'dash-at-point-mode-alist '(cmake-mode . "cmake"))
  (add-to-list 'dash-at-point-mode-alist '(js-mode . "js")))

;; Highlights hexcolors, like #aabbcc and Red.
(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; For programming modes, show delimiters with variying colors to easily
;; distinguish between them.
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlights numbers with another color so they are easier to spot.
(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Highlights thing at point.
(use-package highlight-thing
  :config
  (setq highlight-thing-delay-seconds 1.5)
  (setq highlight-thing-limit-to-defun t) ;; Limit to current function.
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-case-sensitive-p t)

  ;; Enable highlight-thing-mode in prog modes except for modes using LSP because it has it's own
  ;; highlight feature.
  (add-hook 'prog-mode-hook
            #'(lambda ()
                (when (not (member major-mode '(c++-mode c-mode python-mode rust-mode)))
                  (highlight-thing-mode)))))

;; Better commenting DWIM that cycles. Use "C-u M-;" to align comments at end of line with those
;; around it.
(use-package comment-dwim-2
  :config
  (global-set-key (kbd "M-;") 'comment-dwim-2))

;; Avoid escape nightmares by editing string in separate buffer.
(use-package string-edit
  :bind ("C-c e" . string-edit-at-point))

;; Visualize certain like space at end of line and trailing characters after
;; fill column.
(setq whitespace-style '(face tabs lines-tail trailing tab-mark))
(setq whitespace-line-column --global-fill-column)

(add-hook 'prog-mode-hook 'whitespace-mode)

;; Formatting code via clang-format-region.
(use-package clang-format)

;; Highlights escape sequences like \t, \n etc. in strings in programming modes.
(use-package highlight-escape-sequences
  :config
  ;; Has its own `hes-mode-alist' that specifies which modes it supports.
  (hes-mode))

;; ;; Annotate depth when it gets too deep.
;; (use-package annotate-depth
;;   :config
;;   (add-hook 'prog-mode-hook 'annotate-depth-mode)
;;   (add-hook 'annotate-depth-mode-hook
;;             (lambda ()
;;               (if (equal major-mode 'emacs-lisp-mode)
;;                   (setq-local annotate-depth-threshold 10)
;;                 (when (equal major-mode 'c++-mode)
;;                   (setq-local annotate-depth-threshold 5))))))

;; C/C++

(use-package modern-cpp-font-lock
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
              (setq tab-width --general-tab-width))
            (setq c-basic-offset tab-width)
            (setq indent-tabs-mode nil)

            ;; Run clang-format on region or buffer.
            (local-set-key (kbd "C-c f") 'clang-format-region-or-buffer)))

(use-package cc-mode
  :requires key-chord
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

;; ;; Swift
;; (use-package swift-mode
;;   :config
;;   (setq swift-indent-offset --general-tab-width))

;; Elisp

(defhydra netrom-elisp-hydra (:color blue :hint nil)
  ;; Xref
  ("d" xref-find-definitions "Definitions" :column "Xref")
  ("D" xref-find-definitions-other-window "-> other win")
  ("r" xref-find-references "References")
  ("s" xref-find-apropos "Search")
  ("f" netrom/consult-imenu "Filter funcs/classes")
  ("F" netrom/consult-imenu-multi "-> in all buffers")

  ;; Buffer
  ("b" eval-buffer "Eval buffer" :column "Buffer")
  ("r" eval-region "Eval region")

  ;; Misc
  ("q" nil "Cancel" :column "Misc")
  ("b" pop-tag-mark "Back"))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode)
            (local-set-key (kbd "C-c b") 'eval-buffer)
            (local-set-key (kbd "C-c r") 'eval-region)
            (local-set-key (kbd "C-c C-c") 'netrom-elisp-hydra/body)))

;; Auto-compile .el files when loading and saving them.
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Shell script
(require 'sh-script)
(setq-default sh-basic-offset --general-tab-width)

(setq auto-mode-alist
      (append '(("\\.[z]?sh$"  . sh-mode))
              auto-mode-alist))

;; JavaScript
(require 'js)
(setq-default js-indent-level --general-tab-width)

;; It can take quite some time to load dense, minified JS files, so revert to plain old
;; fundamental-mode!
(add-to-list 'auto-mode-alist '("\\.min.js\\'" . fundamental-mode))

(use-package json-mode
  :mode ("\\.json$" . json-mode))

;; Objective-C
(setq auto-mode-alist
      (append '(("\\.mm$" . objc-mode))
              auto-mode-alist))

;; PHP
(use-package php-mode)

;; CSS
(require 'css-mode)
(setq-default css-indent-offset --general-tab-width)

(setq auto-mode-alist
      (append '(("\\.min.css$" . fundamental-mode) ;; Faster to load.
                ("\\.css$" . css-mode)
                ("\\.style$" . css-mode))
              auto-mode-alist))

;; TWIG with SGML/HTML integration.
(use-package twig-mode)

;; Nginx config mode.
(use-package nginx-mode)

;; ;; C#
;; (use-package csharp-mode
;;   :mode (("\\.cs$" . csharp-mode))
;;   :config
;;   (add-hook 'csharp-mode-hook
;;             (lambda ()
;;               (setq c-basic-offset --general-tab-width))))

;; Markdown
(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))

  :config
  ;; Turn off auto-fill-mode beacuse markdown is sensitive about newlines.
  (add-hook 'markdown-mode-hook
            (lambda ()
              (auto-fill-mode 0)
              (visual-line-mode t))))

;; Python
(require 'python)
(setq python-indent-offset 2)

;; Marks TODO, FIXME etc. clearly.
(use-package fic-mode
  :config
  (add-hook 'prog-mode-hook 'fic-mode))

(use-package yasnippet
  :config
  ;; Add local snippets to override some of the defaults in elpa folder.
  (add-to-list 'yas-snippet-dirs --yas-dir)

  ;; (setq yas-prompt-functions
  ;;       '(yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-x-prompt yas-no-prompt))

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

(use-package smartparens
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
  (sp-with-modes '(c-mode c++-mode php-mode java-mode js-mode rust-mode sh-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))

  ;; Same thing but with "[]" and match "|" with "|".
  (sp-with-modes '(rust-mode)
    (sp-local-pair "[" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "|" "|"))

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
(use-package conf-mode
  :mode (("\\.nsi\\'" . conf-windows-mode)
         ("\\.nsis\\'" . conf-windows-mode)))

;; Show vertical lines to guide indentation.
(use-package indent-guide
  :config
  (setq indent-guide-char "|"
        indent-guide-delay 0.5)
  (add-hook 'prog-mode-hook 'indent-guide-mode))

;; ;; Jump to definition for multiple languages without configuration.
;; (use-package dumb-jump
;;   :requires helm
;;   :config
;;   (setq dumb-jump-selector 'helm
;;         dumb-jump-max-find-time 5
;;         dumb-jump-aggressive nil)
;;   (defhydra dumb-jump-hydra (:color blue :columns 3)
;;     "Dumb Jump"
;;     ("j" dumb-jump-go "Go")
;;     ("o" dumb-jump-go-other-window "Other window")
;;     ("e" dumb-jump-go-prefer-external "Go external")
;;     ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
;;     ("i" dumb-jump-go-prompt "Prompt")
;;     ("l" dumb-jump-quick-look "Quick look")
;;     ("b" dumb-jump-back "Back")))

;; ;; Haskell related packages

;; ;; Setup: cabal install hasktags stylish-haskell hoogle
;; ;; It is assumed that PATH is updated to point to where to find Haskell/cabal binaries!
;; (use-package haskell-mode
;;   :config
;;   (setq haskell-tags-on-save t
;;         haskell-process-type 'auto
;;         haskell-process-suggest-remove-import-lines t
;;         haskell-process-auto-import-loaded-modules t
;;         haskell-process-log t)

;;   (defun my-haskell-hoogle-browser ()
;;     "Force hoogle to search via the browser for `haskell-ident-at-point'."
;;     (interactive)
;;     (let ((haskell-hoogle-command nil)) ; `nil' forces usage of browser.
;;       (haskell-hoogle (haskell-ident-at-point))))

;;   ;; TODO: Make hydra
;;   (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
;;   (define-key haskell-mode-map (kbd "C-c f") 'haskell-mode-stylish-buffer)
;;   (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile)
;;   (define-key haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)
;;   (define-key haskell-mode-map (kbd "C-c C-b") 'my-haskell-hoogle-browser)
;;   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;   (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)

;;   (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)

;;   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

;; ;; (use-package company-ghc
;; ;;   :requires company haskell-mode
;; ;;   :config
;; ;;   (setq company-ghc-show-info t)
;; ;;   (add-hook 'haskell-mode-hook 'company-mode)
;; ;;   (add-to-list 'company-backends 'company-ghc))

;; ;; Setup: cabal install hindent
;; (use-package hindent
;;   :requires haskell-mode
;;   :config
;;   (add-hook 'haskell-mode-hook #'hindent-mode))

;; ;; End of Haskell related packages

;; YAML
(use-package yaml-mode
  :mode ("\\.yml$" "\\.yaml$"))

;; Rust

(use-package cargo)

(use-package rust-mode
  :requires cargo hydra
  :config
  (setq rust-format-on-save t)
  (define-key rust-mode-map (kbd "C-c f") 'rust-format-buffer)

  (defun netrom/toggle-rust-backtrace ()
    "Toggle between Rust backtrace enabled and disabled."
    (interactive)
    (if (not cargo-process--enable-rust-backtrace)
        (setq cargo-process--enable-rust-backtrace t)
      (setq cargo-process--enable-rust-backtrace nil)))

  (defun netrom/rust-backtrace-string ()
    (interactive)
    (if (not cargo-process--enable-rust-backtrace)
        "Backtrace: disabled"
      "Backtrace: enabled"))

  (defhydra rust-cargo-hydra ()
    "
%(netrom/rust-backtrace-string)

"
    ("b" cargo-process-build "Build" :column "Cargo")
    ("r" cargo-process-run "Run")
    ("R" cargo-process-run-bin "Run (specific)")
    ("t" cargo-process-test "Test")
    ("c" cargo-process-clean "Clean")

    ("d" cargo-process-doc "Doc" :column "")
    ("D" cargo-process-doc-open "Doc (open)")
    ("u" cargo-process-update "Update")
    ("C" cargo-process-check "Check")
    ("a" cargo-process-audit "Audit")
    ("C-c" cargo-process-clippy "Clippy")

    ("n" next-error "Next" :column "Errors")
    ("N" next-error-skip-warnings "Next, skip warnings")
    ("p" previous-error "Previous")
    ("f" first-error "First")
    ("l" netrom/compilation-last-error "Last")
    ("k" kill-compilation "Stop")

    ("C-b" netrom/toggle-rust-backtrace "Toggle backtrace" :column "Misc")
    ("q" nil "Cancel" :color blue))

  (define-key rust-mode-map (kbd "C-c C-c") 'rust-cargo-hydra/body))

;; Xref

;; Don't show prompt unless nothing is under point or if it has to show it.
(setq-default xref-prompt-for-identifier nil)

(defun netrom/xref-find-apropos-at-point (pattern)
  "Xref find apropos at point, if anything, and show prompt for PATTERN."
  (interactive
   (list
    (read-string "Xref find apropos of: " (thing-at-point 'symbol))))
  (xref-find-apropos pattern))

(use-package flycheck
  :requires hydra flycheck-pycheckers flycheck-inline
  :config
  (progn
    ;; C++11
    (add-hook 'c++-mode-hook
              (lambda ()
                (progn
                  (setq flycheck-clang-language-standard "c++17"
                        flycheck-clang-standard-library "libc++"
                        flycheck-gcc-language-standard "c++17"
                        flycheck-cppcheck-standards '("c++17")
                        flycheck-cppcheck-inconclusive t
                        flycheck-cppcheck-checks '("all")

                        ;; Ignore "no explicit constructor" because often you don't want it to be
                        ;; explicit and in general it's annoying.
                        flycheck-cppcheck-suppressions '("noExplicitConstructor")))))

    ;; Disable elisp checkdoc because it's annoying, and clang/gcc because they never know the
    ;; includes anyway!
    (setq-default flycheck-disabled-checkers
                  '(emacs-lisp-checkdoc c/c++-clang c/c++-gcc))

    (defalias 'fcn 'flycheck-next-error)

    (add-hook 'prog-mode-hook 'flycheck-mode)

    ;; Enable inline errors/warnings/info etc.
    (flycheck-inline-mode)

    ;; Navigate flycheck errors more easily.
    (defhydra flycheck-hydra
      (:pre  (flycheck-list-errors)
             :post (quit-windows-on "*Flycheck errors*")
             :hint nil)
      "Errors"
      ("f"  flycheck-error-list-set-filter                            "Filter")
      ("j"  flycheck-next-error                                       "Next")
      ("k"  flycheck-previous-error                                   "Previous")
      ("gg" flycheck-first-error                                      "First")
      ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
      ("q"  nil))))

(use-package flycheck-inline)

;; NOTE: On Windows, it is important to have `python3.exe' in PATH and not only `python.exe'!
;; Otherwise, flycheck will not detect pycheckers properly.
(when (eq window-system 'w32)
  (let ((p (executable-find "python"))
        (p3 (executable-find "python3")))
    (when (and p (not p3))
      (error "python.exe found but not python3.exe!\npython.exe is located at: %s" p))))

;; Requires local dependencies:
;;   pip install flake8 bandit
(use-package flycheck-pycheckers
  :config
  (setq flycheck-pycheckers-checkers '(flake8 bandit)
        flycheck-pycheckers-ignore-codes
        '("C0411" "C0413" "C0103" "C0111" "W0142" "W0201" "W0232" "W0403" "W0511" "E1002" "E1101"
          "E1103" "R0201" "R0801" "R0903" "R0904" "R0914" "W503" "W504"
          ;; flake8
          "E111" "E114" "E121" "E126" "E127" "E221" "E241" "E302" "E305"
          ;; bandit
          "B101" "B322")
        flycheck-pycheckers-max-line-length 100
        flycheck-pycheckers-multi-thread "true")

  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)

  ;; Ensure that the correct python checker is chosen.
  (add-hook 'python-mode-hook (lambda () (flycheck-select-checker 'python-pycheckers))))

;; Requires Rust to be installed locally: install rustup
(use-package flycheck-rust
  :requires flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Language Server Protocol

;; Language support:
;;
;; == C++ ==
;; Requires clangd is installed (comes with clang).
;;
;; clangd tries to locate the "compile_commands.json" file in the root of the project, so it's
;; useful to make a symlink in the project root and to where it's located in a build folder.
;;
;; == Rust ==
;; Requires Rust Language Server (rls) to be installed.
;; Installation:
;; 1. rustup update
;; 2. rustup component add rls rust-analysis rust-src
;;
;; == Python ==
;; Requires python-language-server:
;;   pip install python-language-server
;;
;; == PHP ==
;; Requires the php-language-server that is bundled with the emacs config, and was installed like
;; this:
;; 1. Created folder ~/.composer
;; 2. Created file "composer.json" in that folder with the contents:
;;    {
;;      "minimum-stability": "dev",
;;      "prefer-stable": true
;;    }
;; 3. Installed "composer" on the system, like: brew install composer
;; 4. And while inside "~/.composer/" executing:
;;    composer require felixfbecker/language-server
;;    composer run-script --working-dir=vendor/felixfbecker/language-server parse-stubs


(let ((straight-current-profile 'pinned))
  (use-package lsp-mode
    :requires hydra markdown-mode
    :config
    (setq lsp-prefer-flymake nil ;; Prefer using lsp-ui (flycheck) over flymake.
          lsp-enable-xref t)

    ;; Let clangd use half of the logical cores but one as minimum. `--background-index' requires
    ;; clangd v8+! Enable clang-tidy checks, too.
    (setq lsp-clients-clangd-args `(,(format "-j=%d" (max 1 (/ (system-cores :logical t) 2)))
                                    "--background-index" "--clang-tidy" "--log=error"))

    (add-hook 'c++-mode-hook #'lsp)
    (add-hook 'rust-mode-hook #'lsp)
    (add-hook 'python-mode-hook #'lsp)
    (add-hook 'php-mode-hook #'lsp)

    (setq netrom--general-lsp-hydra-heads
          '(;; Xref
            ("d" xref-find-definitions "Definitions" :column "Xref")
            ("D" xref-find-definitions-other-window "-> other win")
            ("r" xref-find-references "References")

            ;; Peek
            ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
            ("C-r" lsp-ui-peek-find-references "References")
            ("C-i" lsp-ui-peek-find-implementation "Implementation")

            ;; LSP
            ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
            ("C-a" lsp-execute-code-action "Execute code action")
            ("R" lsp-rename "Rename")
            ("t" lsp-goto-type-definition "Type definition")
            ("i" lsp-goto-implementation "Implementation")
            ("f" consult-imenu "Filter funcs/classes")
            ("F" consult-imenu-multi "-> in all buffers")
            ("s" consult-lsp-file-symbols "Search file symbols")
            ("S" consult-lsp-symbols "Search workspace symbols")
            ("M-d" consult-lsp-diagnostics "Diagnostics")
            ("C-c" lsp-describe-session "Describe session")

            ;; Flycheck
            ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck"))

          netrom--misc-lsp-hydra-heads
          '(;; Misc
            ("q" nil "Cancel" :column "Misc")
            ("b" pop-tag-mark "Back")))

    ;; Create general hydra.
    (eval `(defhydra netrom/lsp-hydra (:color blue :hint nil)
             ,@(append
                netrom--general-lsp-hydra-heads
                netrom--misc-lsp-hydra-heads)))

    (add-hook 'lsp-mode-hook
              (lambda () (local-set-key (kbd "C-c C-l") 'netrom/lsp-hydra/body))))

  (use-package lsp-ui
    :requires lsp-mode flycheck
    :config

    (setq lsp-ui-doc-enable nil
          ;; lsp-ui-doc-use-childframe t
          ;; lsp-ui-doc-position 'top
          ;; lsp-ui-doc-include-signature t
          lsp-ui-sideline-enable nil
          lsp-ui-flycheck-enable t
          lsp-ui-flycheck-list-position 'right
          lsp-ui-flycheck-live-reporting t
          lsp-ui-peek-enable t
          lsp-ui-peek-list-width 60
          lsp-ui-peek-peek-height 25)

    ;; Remap keys for xref find defs to use the LSP UI peek mode.
    ;;(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    ;;(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

    (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

;; Turn on smerge-mode when opening a file with the markers in them.
(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge)

;;;;; Project Handling ;;;;;

(use-package ag
  :config
  (setq ag-arguments '("--nocolor" "--nogroup" "--smart-case")
        ag-ignore-list '("*.min.js" "*.min.css")))

(use-package projectile
  :requires hydra ag
  :init
  (defun netrom/projectile-mode-line ()
    "Report project name in the mode line."
    (if (file-remote-p default-directory)
        "rρ"
      (if (string-equal "-" (projectile-project-name))
          "!ρ"
        "ρ")))
  (setq projectile-keymap-prefix (kbd "C-x p")
        projectile-enable-caching t
        projectile-mode-line-prefix "ρ"
        projectile-mode-line-function 'netrom/projectile-mode-line
        projectile-switch-project-action 'projectile-find-file)
  :config
  (defun netrom/projectile-switch-project-magit (args)
    "Switch to project using projectile and run `magit-status'."
    (interactive "P")
    (let ((projectile-switch-project-action 'magit-status))
      (projectile-switch-project)))

  (defhydra projectile-hydra (:idle 1 :hint nil)
    "
Projectile: %(projectile-project-root)

     Find                Search                Buffers                Cache/Project
-------------------------------------------------------------------------------------------
  _f_: File            _ss_: Ag (at point)      _b_: Switch to buffer    _p_: Switch project (find file)
  _F_: File dwim       _sf_: Ag (choose dir)    _k_: Kill all buffers    _m_: Switch project (magit)
  _o_: Other file      _sr_: Ripgrep (async)                           ^^_c_: Cache clear
  _r_: Recent file     _sR_: Ripgrep (cur dir)                         ^^_x_: Remove known project
  _d_: Dir             _sg_: Gip Grep (async)                          ^^^^_X_: Cleanup non-existing
  _w_: File other win  _sG_: Gip Grep (cur dir)                        ^^^^_z_: Cache current file

"
    ("f" projectile-find-file)
    ("F" projectile-find-file-dwim)
    ("o" projectile-find-other-file)
    ("r" projectile-recentf)
    ("d" projectile-find-dir)
    ("w" projectile-find-file-other-window)

    ("ss" projectile-ag :color blue)
    ("sf" ag-regexp :color blue)
    ("sr" netrom/consult-ripgrep :color blue)
    ("sR" netrom/consult-ripgrep-current-dir :color blue)
    ("sg" netrom/consult-git-grep :color blue)
    ("sG" netrom/consult-git-grep-current-dir :color blue)

    ("b" projectile-switch-to-buffer)
    ("k" projectile-kill-buffers)

    ("p" projectile-switch-project)
    ("m" netrom/projectile-switch-project-magit :color blue)
    ("c" projectile-invalidate-cache)
    ("z" projectile-cache-current-file)
    ("x" projectile-remove-known-project)
    ("X" projectile-cleanup-known-projects)

    ("M" magit-status "Magit" :color blue)
    ("q" nil "Cancel" :color blue))

  (define-key projectile-mode-map projectile-keymap-prefix 'projectile-hydra/body)

  (projectile-mode))

;;;;; Version Control ;;;;;

;; Add Git-for-Windows' /usr/bin to PATH because it also has diff.exe that is needed for diffing.
(let ((fld "C:/Program Files/Git/usr/bin"))
  (when (file-directory-p fld)
    (push fld exec-path)
    (setenv "PATH" (concat fld ";" (getenv "PATH")))))

;; Highlight uncommitted changes/additions/deletions in the fringe.
(use-package diff-hl
  :requires magit
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)

  (diff-hl-flydiff-mode 1)
  (global-diff-hl-mode 1))

(use-package git-timemachine
  :straight (git-timemachine :type git :host codeberg :repo "pidu/git-timemachine")
  :config
  (defalias 'tm 'git-timemachine))

;;;;; Editing ;;;;;

(use-package ace-mc)

(use-package multiple-cursors
  :requires hydra ace-mc
  :init
  (setq mc/list-file (concat --misc-dir "mc-lists.el"))
  :config
  (setq mc/mode-line
        (quote
         (" ["
          (:eval (format #("%d" 0 2 (face font-lock-string-face)) (mc/num-cursors)))
          " mc] ")))

  ;; Make <return> insert newline at each cursor instead of ending mc mode.
  (define-key mc/keymap (kbd "<return>") nil)


  (defhydra netrom/mc-hydra ()
    "
Multiple Cursors

"
    ;; Next
    ("n" mc/mark-next-like-this "Next" :column "Next")
    ("u" mc/unmark-next-like-this "Unmark")
    ("s" mc/skip-to-next-like-this "Skip")

    ;; Previous
    ("p" mc/mark-previous-like-this "Previous" :column "Previous")
    ("U" mc/unmark-previous-like-this "Unmark")
    ("S" mc/skip-to-previous-like-this "Skip")

    ;; Cycling
    ;; NOTE: It is in purpose that "C-s" `isearch-forward' and "C-r" `isearch-backward' are replaced
    ;; because isearch isn't supported in mc so it might as well do something familiar!
    ("C-s" mc/cycle-forward "Forward" :column "Cycling")
    ("C-r" mc/cycle-backward "Backward")

    ;; Insert
    ("C-n" mc/insert-numbers "Numbers" :column "Insert")
    ("C-l" mc/insert-letters "Letters")

    ;; "Like This"
    ("a" mc/mark-all-like-this-dwim "All (dwim)" :column "Like This")
    ("d" mc/mark-all-like-this-in-defun "All (defun)")
    ("w" mc/mark-all-words-like-this-in-defun "All words (defun)")
    ("M-s" mc/mark-all-symbols-like-this-in-defun "All symbols (defun)")

    ;; Misc
    ("m" mc/mark-all-dwim "All (dwim)" :column "Misc")
    ("l" mc/edit-lines "Edit lines")
    ("r" mc/mark-all-in-region "Mark in region")
    ("R" mc/mark-all-in-region-regexp "Mark in region (regexp)")
    ("A" ace-mc-add-multiple-cursors "Ace" :color blue)
    ("q" nil "Quit" :color blue))

  (global-set-key (kbd "C-c m") 'netrom/mc-hydra/body)

  ;; Trigger hydra via some of the normal bindings, too.
  (global-set-key (kbd "C-c n") 'netrom/mc-hydra/mc/mark-next-like-this)
  (global-set-key (kbd "C-c p") 'netrom/mc-hydra/mc/mark-previous-like-this)
  (global-set-key (kbd "C-c a") 'netrom/mc-hydra/mc/mark-all-like-this-dwim)
  (global-set-key (kbd "C-c l") 'netrom/mc-hydra/mc/edit-lines))

(use-package expand-region
  :config
  (global-set-key (kbd "M-m") 'er/expand-region))

;; Convenient word transformations.
(use-package fix-word
  :config
  (global-set-key (kbd "M-u") 'fix-word-upcase)
  (global-set-key (kbd "M-l") 'fix-word-downcase)
  (global-set-key (kbd "M-c") 'fix-word-capitalize))

;; Better fill/unfill that is toggleable.
(use-package unfill
  :config
  (global-set-key (kbd "M-q") 'unfill-toggle))

(use-package goto-last-change
  :bind ("C-x /" . goto-last-change))

;; Loads ".editorconfig" files and sets up indent style/size etc.
;; Ref: https://editorconfig.org/
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;;;;; Copying ;;;;;

;; Convenience for formatting things to be copied and inserted elsewhere, like code blocks.
(use-package copy-as-format
  :requires hydra
  :config

  (defhydra copying-hydra (:color blue :columns 5 :idle 1)
    "Copy as format"
    ("s" copy-as-format-slack "slack")
    ("j" copy-as-format-jira "jira")
    ("m" copy-as-format-markdown "markdown")
    ("g" copy-as-format-github "github")
    ("l" copy-as-format-gitlab "gitlab")
    ("h" copy-as-format-html "html")
    ("o" copy-as-format-org-mode "org-mode")
    ("w" copy-as-format-mediawiki "mediawiki")
    ("b" copy-as-format-bitbucket "bitbucket")
    ("r" copy-as-format-rst "rst")
    ("p" copy-as-format-pod "pod")
    ("c" copy-as-format-hipchat "hipchat")
    ("d" copy-as-format-disqus "discus"))

  (global-set-key (kbd "C-c w") 'copying-hydra/body))

;;;;; Spelling ;;;;;

(use-package ispell
  :config
  (if (eq window-system 'w32)
      (progn
        ;; Install dictionaries from here:
        ;;   https://github.com/wooorm/dictionaries?tab=readme-ov-file#list-of-dictionaries
        ;; Put them in "C:/Hunspell" as "en_US.aff" and "en_US.dic" etc.
        (setq ispell-program-name "hunspell")

        (setq ispell-hunspell-dictionary-alist
              '(("en_US" "[A-Za-z]" "[^A-Za-z]" "[']" nil ("-d" "en_US") nil utf-8)
                ("da_DK" "[A-Åa-å]" "[^A-Åa-å]" "[']" nil ("-d" "da_DK") nil utf-8)))

        (defun da-spell ()
          "Set ispell to use Danish dictionary (locally)"
          (interactive)
          (setenv "LANG" "da_DK")
          (ispell-change-dictionary "da_DK"))

        (defun en-spell ()
          "Set ispell to use English dictionary (locally)"
          (interactive)
          (setenv "LANG" "en_US")
          (ispell-change-dictionary "en_US"))

        (en-spell))

    (progn
      (setq ispell-program-name "aspell")

      ;; Speed up aspell: ultra | fast | normal
      (setq ispell-extra-args '("--sug-mode=normal")))

    (defun da-spell ()
      "Set ispell to use Danish dictionary (locally)"
      (interactive)
      (ispell-change-dictionary "dansk"))

    (defun en-spell ()
      "Set ispell to use English dictionary (locally)"
      (interactive)
      (ispell-change-dictionary "english")))

  (defalias 'sb 'ispell-buffer)

  ;; C-x C-i to ispell word and save value as abbrev globally so it can be completed again using
  ;; dabbrev-expand or hippie-expand, for instance. Otherwise it keeps looking for spelling mistakes
  ;; backwards.
  (defun endless/simple-get-word ()
    (car-safe (save-excursion (ispell-get-word nil))))

  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will be global.
If there's nothing wrong with the word at point, keep looking for
a typo until the beginning of buffer. You can skip typos you
don't want to fix with `SPC', and you can abort completely with
`C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (endless/simple-get-word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word)
          (backward-char))
        (setq aft (endless/simple-get-word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point"))))

  (global-set-key (kbd "C-x C-i") 'endless/ispell-word-then-abbrev))

(use-package flyspell
  :requires ispell flyspell-lazy auto-dictionary
  :config
  ;; NOTE: Automatic spellchecking is disabled because it makes everything terribly slow if the
  ;; files is very long.
  (defun enable-spelling ()
    "Enable spellchecking and automatic dictionary detection."
    (interactive)
    (progn
      (flyspell-lazy-mode)
      (flyspell-mode)
      (auto-dictionary-mode)))

  (defun disable-spelling ()
    (interactive)
    (progn
      (flyspell-lazy-mode -1)
      (flyspell-mode -1)
      (auto-dictionary-mode -1)))

  ;; ;; Flyspell activation for text mode.
  ;; (add-hook 'text-mode-hook (lambda ()
  ;;                             (flyspell-lazy-mode)
  ;;                             (flyspell-mode)))

  ;; ;; Remove Flyspell from some sub modes of text mode
  ;; (dolist (hook '(change-log-mode-hook
  ;;                 log-edit-mode-hook
  ;;                 nxml-mode-hook))
  ;;   (add-hook hook (lambda ()
  ;;                    (flyspell-lazy-mode -1)
  ;;                    (flyspell-mode -1))))

  ;; Flyspell comments and strings in programming modes.
  ;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

;; Improve flyspell responsiveness using idle timers.
(use-package flyspell-lazy)

;; Disabled due to not being used and slows init a bit: 155 ms
;; Tries to automatically detect the language of the buffer and setting the dictionary accordingly.
;; (use-package auto-dictionary
;;   :requires ispell
;;   :config
;;   ;;(add-hook 'text-mode-hook 'auto-dictionary-mode)
;;   )

;;;;; File System ;;;;;

(require 'dired)
(require 'ls-lisp)

(defalias 'qrd 'find-name-dired)
(defalias 'qrrd 'find-name-dired)

;; Show human-readable sizes and show folders with "/" at the end.
(setq dired-listing-switches "-lhaF")

;; In dired mode, use Emacs's emulation of "ls" because the system one most
;; often doesn't support the "--dired" argument.
(setq ls-lisp-use-insert-directory-program nil)

;; Show directories first.
(setq ls-lisp-dirs-first t)

;; Don't show link count but uid+gid.
(setq ls-lisp-verbosity '(uid gid))

;; Updated dired buffers on file system changes.
(add-hook 'dired-mode-hook 'auto-revert-mode)

(key-chord-define dired-mode-map "qq" 'dired-up-directory)

;; Unwanted files to be flagged for deletion on dired-flag-garbage-files or "%&".
(setq dired-garbage-files-regexp
      "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|elc\\)\\)\\'")

(define-key dired-mode-map "k" 'dired-do-delete)
(define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)

;; Enter editable (wdired) mode where file and folder names can be changed directly as a buffer.
(define-key dired-mode-map "E" 'wdired-change-to-wdired-mode)

;; C-a goes to start of file entry (column 2).
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))
(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

;; M-< goes to first file, the fourth line unless in omit mode then second line.
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (if (bound-and-true-p dired-omit-mode)
      (dired-next-line 2)
    (dired-next-line 4)))
(define-key dired-mode-map (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(define-key dired-mode-map (kbd "M-p") 'dired-back-to-top)

;; M-> goes to last file.
(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))
(define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
(define-key dired-mode-map (kbd "M-n") 'dired-jump-to-bottom)

;; Easier usage of dired by using "." in dired buffer.
(defhydra dired-hydra (:hint nil)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'dired-hydra/body)

(key-chord-define dired-mode-map "ss" 'dired-sort-size)
(key-chord-define dired-mode-map "se" 'dired-sort-extension)
(key-chord-define dired-mode-map "sc" 'dired-sort-ctime)
(key-chord-define dired-mode-map "su" 'dired-sort-utime)
(key-chord-define dired-mode-map "st" 'dired-sort-time)
(key-chord-define dired-mode-map "sn" 'dired-sort-name)

(use-package dired-narrow
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;; Open a temp file with a certain file extension.
(use-package find-temp-file
  :bind ("C-x C-t" . find-temp-file))

;;;;; Consult ;;;;;

(defun netrom/consult-imenu ()
  (interactive)
  (consult-imenu))

(defun netrom/consult-imenu-multi ()
  (interactive)
  (consult-imenu-multi))

(defun netrom/consult-compile-error ()
  (interactive)
  (consult-compile-error))

(defun netrom/consult-outline ()
  (interactive)
  (consult-outline))


(defun netrom/make-consult-search-funcs (name)
  "Creates 6 functions given NAME:
`netrom/consult-NAME' with optional arguments `current-dir' and `symbol-at-point'
`netrom/consult-NAME-symbol-at-point'
`netrom/consult-NAME-current-dir'
`netrom/consult-NAME-current-dir-symbol-at-point'
`netrom/consult-NAME-ask-dir'
`netrom/consult-NAME-ask-dir-symbol-at-point'"
  (progn
    (eval (list 'defun
                (intern (format "netrom/consult-%s" name))
                (list '&optional 'current-dir 'symbol-at-point)
                (list 'interactive)
                (list (intern (format "consult-%s" name))
                      (list 'when 'current-dir (list 'current-directory))
                      (list 'when 'symbol-at-point (list 'thing-at-point ''symbol)))))
    (eval (list 'defun
                (intern (format "netrom/consult-%s-symbol-at-point" name))
                ()
                (list 'interactive)
                (list (intern (format "netrom/consult-%s" name)) 'nil 't)))
    (eval (list 'defun
                (intern (format "netrom/consult-%s-current-dir" name))
                ()
                (list 'interactive)
                (list (intern (format "netrom/consult-%s" name)) 't)))
    (eval (list 'defun
                (intern (format "netrom/consult-%s-current-dir-symbol-at-point" name))
                ()
                (list 'interactive)
                (list (intern (format "netrom/consult-%s" name)) 't 't)))
    (eval (list 'defun
                (intern (format "netrom/consult-%s-ask-dir" name))
                (list 'dir '&optional 'symbol-at-point)
                (list 'interactive "DSearch in:\nP")
                (list (intern (format "consult-%s" name))
                      'dir
                      (list 'when 'symbol-at-point (list 'thing-at-point ''symbol)))))
    (eval (list 'defun
                (intern (format "netrom/consult-%s-ask-dir-symbol-at-point" name))
                ()
                (list 'interactive)
                (list 'let (list (list 'current-prefix-arg 't))
                      (list 'call-interactively
                            `(quote ,(intern (format "netrom/consult-%s-ask-dir" name)))))))))


(dolist (name '(grep ripgrep git-grep))
  (netrom/make-consult-search-funcs name))


(defun netrom/consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(use-package consult-lsp)

(use-package consult-projectile
  :requires projectile)

(use-package consult
  :requires projectile selectrum
  :bind (;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)
         ("C-x r" . consult-recent-file)
         ;; Other custom bindings
         ("<help> a" . describe-symbol)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . netrom/consult-grep-symbol-at-point)
         ("M-s G" . netrom/consult-git-grep-symbol-at-point)
         ("M-s r" . netrom/consult-ripgrep-symbol-at-point)
         ("M-s l" . netrom/consult-line-symbol-at-point)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s k" . consult-kmacro)
         ("M-s M" . consult-kmacro)
         :map isearch-mode-map
         ("M-l" . consult-line)
         ("M-L" . consult-line-multi)
         ("M-h" . consult-isearch-history))
  :init
  (setq consult-project-root-function #'projectile-project-root
        consult-goto-line-numbers nil

        ;; Not using splitting due to how Orderless works.
        consult-async-split-style nil

        ;; Xref through consult.
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Auto-preview with a delay while browsing themes since it is a bit slow previewing them.
  (consult-customize consult-theme :preview-key '(:debounce 0.5 any)))

;;;;; Search ;;;;;

(defun netrom/isearch-yank-region-or-thing-at-point ()
  "Pull active region or thing-at-point from buffer into serach
string. If region was active then `deactivate-mark' is invoked to
stop region from expanding to next search match."
  (interactive)
  (isearch-yank-string
   (if (region-active-p)
       (let ((reg (buffer-substring (region-beginning) (region-end))))
         (deactivate-mark)
         reg)
     (thing-at-point 'symbol))))

;; Yank into search buffer the active region or thing-at-point instead of next word or char via
;; `isearch-yank-word-or-char'.
(define-key isearch-mode-map (kbd "C-w") 'netrom/isearch-yank-region-or-thing-at-point)

;; Sometimes the cursor should be at the opposite end of the search match, e.g. when searching
;; forward the cursor will be at the end of the match, but if the opposite is intended then exit
;; isearch mode using C-RET. Same thing for searching backwards where it puts the cursor at the end
;; of the match instead.
;; (defun isearch-exit-other-end ()
;;   "Exit isearch, at the opposite end of the string."
;;   (interactive)
;;   (isearch-exit)
;;   (goto-char isearch-other-end))
;; (define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

;; (defun zap-to-isearch (beg end)
;;   "Kill the region between the mark and the closest portion of
;; the isearch match string."
;;   (interactive "r")
;;   (when (not mark-active)
;;     (error "Mark is not active"))
;;   (save-excursion
;;     (let* ((isearch-bounds (list isearch-other-end (point)))
;;            (ismin (apply 'min isearch-bounds))
;;            (ismax (apply 'max isearch-bounds)))
;;       (if (< (mark) ismin)
;;           (kill-region (mark) ismin)
;;         (if (> (mark) ismax)
;;             (kill-region ismax (mark))
;;           (error "Internal error in isearch kill function.")))
;;       (isearch-exit))))

;; (define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)

(use-package avy
  :requires hydra
  :config
  (defhydra avy-hydra (:color blue :columns 4)
    ("g" avy-goto-line "Line")
    ("M-g" avy-goto-line "Line")
    ("p" avy-goto-line-above "Line above")
    ("n" avy-goto-line-below "Line below")
    ("c" avy-goto-char-2 "Char-2")
    ("C" avy-goto-char "Char")
    ("w" avy-goto-word-1 "Word")
    ("s" avy-goto-subword-1 "Subword")
    ("l" avy-goto-char-in-line "Char in line")
    ("i" netrom/consult-imenu "Imenu")
    ("I" netrom/consult-imenu-multi "Imenu multi")
    ("e" netrom/consult-compile-error "Compile error")
    ("o" netrom/consult-outline "Outline")
    ("," avy-pop-mark "Pop mark"))
  (global-set-key (kbd "M-g") 'avy-hydra/body))

(use-package deadgrep
  :config
  (defun netrom/deadgrep-postpone-start ()
    "Deadgrep shows the search buffer but doesn't start the
search when the prefix argument is defined."
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'deadgrep)))

  (defalias 'dg 'deadgrep)
  (defalias 'dgp 'netrom/deadgrep-postpone-start))

;;;;; Session ;;;;;

(use-package savehist
  :config
  ;; Saves mini buffer history including search and kill ring values, compile history.
  (setq savehist-additional-variables
        '(search-ring
          regexp-search-ring
          query-replace-history
          kill-ring
          compile-history
          ))
  (setq savehist-autosave-interval 60)
  (setq savehist-file (concat user-emacs-directory "savehist"))
  (savehist-mode t))

(use-package recentf
  :config
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-save-file (concat user-emacs-directory "recentf")
        recentf-auto-cleanup 300        ; Cleanup every 5 minutes of idle time.
        recentf-exclude '("ido\\.last"
                          "\\.emacs\\.d/saveplace"
                          "\\.emacs\\.d/savehist"
                          "\\.emacs\\.d/recentf"
                          "\\.emacs\\.d/prescient-save"
                          "\\.git/COMMIT_EDITMSG"
                          ".*-autoloads\\.el"
                          "/elpa/.*"))
  (recentf-mode 1)
  ;; (global-set-key "\C-xr" 'recentf-open-files)
  )

;; Saves cursor positions of visited files.
(use-package saveplace
  :config
  (setq save-place-limit 400)
  (setq save-place-file (concat user-emacs-directory "saveplace"))
  (save-place-mode t))

;;;;; LaTeX ;;;;;

(add-hook 'latex-mode-hook 'auto-fill-mode)
(setenv "TEXINPUTS" ".:~/latex/:")

;;;;; KeyShot related ;;;;;

;; Associate `ksnrlog-mode' with KS NR log files to fontify them.
(add-to-list 'auto-mode-alist
             '("\\(render\\|configurator\\|monitor\\|watchdog\\)_.+\\.log\\'" . ksnrlog-mode))

;; `crashpad-stack-mode' which is useful for fontifying call stacks created by crashpad/breakpad
;; minidump_stackwalk (or minidump_analysis.py).
;; TODO: Might be too general?
(add-to-list 'auto-mode-alist '(".*stack.*\\.txt\\'" . crashpad-stack-mode))

;; Wraps a function with // ***.. before and after (the region selected). Both
;; inserted lines with have a length fo 80 characters.
(defun ks-wrap-function (start end)
  "Put comments around KeyShot function."
  (interactive "r")
  (let ((str (concat "// " (make-string (- --global-fill-column 3) ?*) "\n")))
    (save-excursion
      (goto-char end)
      (insert str)
      (goto-char start)
      (insert str))))

(defun ks-fix-function-comments ()
  "Fix all functions with an incorrect number of '// ***..' (or '=' or '-') around them."
  (interactive)
  (let* ((regexp "[ ]*\/\/[ ]*[\*\=\-]+")
         (line-width --global-fill-column)
         (str (concat "// " (make-string (- line-width 3) ?*)))
         (old-line)
         (line-end))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at regexp)
          ;; Replace with correct line.
          (beginning-of-line)
          (kill-line)
          (insert str)

          ;; Indent to fit with sorrounded scopes, if any.
          (indent-for-tab-command)

          ;; If the line exceeds `line-width` then kill the rest of line.
          (end-of-line)
          (setq line-end (current-column))
          (beginning-of-line)
          (setq old-line (line-number-at-pos))
          (forward-char line-width)
          (when (and (< (current-column) line-end)
                     (= old-line (line-number-at-pos)))
            (kill-line))
          (goto-line old-line))
        (forward-line))))) ;; Search next line.

(defun ks-fix-function-curls ()
  "Fix all functions with '// ***..' around it to have it's '{' be put after the second '//***..'."
  (interactive)
  (let* ((regexp-line "[ ]*\/\/[ ]*[\*]+")
         (regexp-curl "{")
         (line-width --global-fill-column)
         (str (concat "// " (make-string (- line-width 3) ?*)))
         (line-one)
         (line-two)
         (flag nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not (eobp))
                  (not flag))
        ;; Find line one.
        (while (and (not (eobp))
                    (not flag))
          (when (looking-at regexp-line)
            (setq line-one (line-number-at-pos))
            (setq flag t))
          (forward-line))

        (unless (eobp)
          (setq flag nil)
          (forward-line)

          ;; Find line two.
          (while (and (not (eobp))
                      (not flag))
            (when (looking-at regexp-line)
              (setq line-two (line-number-at-pos))
              (setq flag t))
            (forward-line))

          (unless (eobp)
            (setq flag nil)
            (goto-line line-one)

            ;; Find and remove the '{'.
            (while (and (<= (line-number-at-pos) line-two)
                        (not flag))
              (when (looking-at regexp-curl)
                (delete-char 1)
                (cycle-spacing 0) ;; Remove any whitespace
                (setq flag t))
              (forward-char 1))

            (unless (eobp)
              (goto-line line-two)

              ;; If deleted '{' then insert on new line after `line-two`.
              (when flag
                (end-of-line)
                (insert "\n")
                (insert regexp-curl))

              (setq flag nil)
              (forward-line))))))))

(defun ks-fix-buffer ()
  (interactive)
  (ks-fix-function-curls)
  (cleanup-region-or-buffer)
  (ks-fix-function-comments)
  (clang-format-buffer))

;; Bindings
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-M-l") 'ks-wrap-function)))

;;;;; User Interface ;;;;;

;; on-screen mode to track reading markers.
(use-package on-screen
  :config
  (setq on-screen-highlight-method (quote fringe))
  (global-on-screen-mode 1))

(use-package window-numbering)

;; Shows the number of matches for searches.
(use-package anzu
  :config
  (defalias 'qr 'anzu-query-replace)
  (defalias 'qrr 'anzu-query-replace-regexp)

  (setq
   ;; Don't add to modeline because doom-modeline will show anzu.
   anzu-cons-mode-line-p nil

   ;; Deactivate region, if any, when using anzu replace functionality because it's hard to see the
   ;; search results with an active region as well.
   anzu-deactivate-region t)

  ;; Use anzu to query-replace via isearch.
  (define-key isearch-mode-map (kbd "M-r") 'anzu-isearch-query-replace)
  (define-key isearch-mode-map (kbd "M-R") 'anzu-isearch-query-replace-regexp)

  ;; Change the mode-line text summary of search/replace results.
  ;; Note: doom-modeline will use its own rendering so this function only takes effect when
  ;; doom-modeline-mode isn't active!
  (defun netrom/anzu-update-func (here total)
    (when anzu--state
      (let ((status (cl-case anzu--state
                      (search (format "(%s/%d%s)"
                                      (anzu--format-here-position here total)
                                      total (if anzu--overflow-p "+" "")))
                      (replace-query (format "(%d replaces)" total))
                      (replace (format "(%d/%d)" here total))))
            (face (if (and (zerop total) (not (string= isearch-string "")))
                      'anzu-mode-line-no-match
                    'anzu-mode-line)))
        (propertize status 'face face))))
  (setq anzu-mode-line-update-function #'netrom/anzu-update-func)

  (global-anzu-mode t))

;; (use-package spaceline
;;   :requires window-numbering
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

(let ((straight-current-profile 'pinned))
  (use-package doom-modeline
    :requires (window-numbering anzu)
    :hook (after-init . doom-modeline-mode)
    :config
    (window-numbering-mode t)

    (setq doom-modeline-minor-modes nil
          doom-modeline-enable-word-count t
          doom-modeline-checker-simple-format t
          doom-modeline-buffer-file-name-style 'truncate-upto-project
          doom-modeline-env-python-executable "python3"
          doom-modeline-indent-info t)))

;; Remove or rename mode line values.
(use-package diminish
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
      '(diminish 'global-auto-revert-mode "ar")
      '(diminish 'global-auto-composition-mode "ar")))

  (eval-after-load "rainbow-mode"
    '(diminish 'rainbow-mode))

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

  (eval-after-load "auto-dim-other-buffers"
    '(diminish 'auto-dim-other-buffers-mode))

  (eval-after-load "indent-guide"
    '(diminish 'indent-guide-mode))

  (eval-after-load "beacon"
    '(diminish 'beacon-mode)))

;; Line numbers. Is faster than the built-in linum mode.
(require 'linum)
(use-package nlinum
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

;; Show one buffer and hiding all others, do again to restore buffers.
(use-package zygospore
  :config
  (global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows))

;; Visually makes non-current buffers less prominent.
(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode t))

;; Dashboard shows recent files and projects in a startup buffer.
(use-package dashboard
  :config
  (setq dashboard-startup-banner 2 ; text "Emacs" banner
        dashboard-items '((recents  . 10)
                          (projects . 10)))
  (dashboard-setup-startup-hook))

;; Makes the cursor stay vertically centered (unless being at the top or bottom part).
(use-package centered-cursor-mode
  :config
  (defalias 'ccm 'centered-cursor-mode)
  (defalias 'gccm 'global-centered-cursor-mode))

;;;;; Notes ;;;;;

(use-package markdown-mode
  :bind (:map markdown-mode-map
        ("S-<return>" . markdown-toggle-gfm-checkbox))
  :config
  ;; Syntax highlighting of wiki links.
  (setq markdown-enable-wiki-links t))

;; (use-package obsidian
;;   :straight (obsidian :type git :host github :repo "licht1stein/obsidian.el")
;;   :requires hydra
;;   :ensure t
;;   :config
;;   (obsidian-specify-path "~/Library/Mobile Documents/iCloud~md~obsidian/Documents/notes")
;;   (global-obsidian-mode t)
;;   :bind (:map obsidian-mode-map
;;               ("C-c M-o" . 'obsidian-hydra/body)  ;; Show hydra menu.

;;               ;; Shadows comparable functions of markdown-mode-map.
;;               ("C-c C-o" . 'obsidian-hydra/obsidian-follow-link-at-point)
;;               ("C-c C-l" . 'obsidian-insert-wikilink)))

;;;;; Miscellaneous ;;;;;

;; Set auto-fill-mode and org minor modes for lists and tables.
(add-hook 'text-mode-hook (lambda () (auto-fill-mode t)))

;; Shows available key bindings in a completing, helpful way. It's very useful if one cannot
;; remember the entire command sequence or to discover what's possible.
(use-package which-key
  :config
  (setq which-key-idle-delay 2              ; Don't show too fast.
        which-key-idle-secondary-delay 0.05 ; Show next possibilities immediately when shown.
        which-key-show-early-on-C-h t)      ; Show instantly while waiting in idle delay.
  (which-key-mode))

;; Disabled due to not being used and slows init a bit:
;;   dep yabin: 87 ms
;;   dep calc:  40 ms
;; (use-package describe-number
;;   :config
;;   (global-set-key (kbd "M-?") 'describe-number-at-point))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Using very-large-file mode for large files without asking.
(use-package vlf
  :config
  (require 'vlf-setup)
  (setq vlf-application 'dont-ask))


;;;;;;;;;;;;;;;;;;;
;; End of Config ;;
;;;;;;;;;;;;;;;;;;;

(loading-done)
