;;; Emacs Configurations                              -*- no-byte-compile: t -*-

(let ((major 25)
      (minor 1))
  (unless (and (>= emacs-major-version major)
               (>= emacs-minor-version minor))
    (error "Emacs v. %d.%d+ is required for this configuration!" major minor)))

;; Constants.
(defconst emacs-start-time (current-time))
(defconst init-dir (concat user-emacs-directory "init.d/"))
(defconst yas-dir (concat user-emacs-directory "snippets/"))
(defconst themes-dir (concat user-emacs-directory "themes/"))
(defconst user-cache-dir (concat user-emacs-directory ".cache/"))

;; These are without .el because `load` will add these as appropriately when using them.
(defconst general-file (concat user-emacs-directory "general"))
(defconst functions-file (concat user-emacs-directory "functions"))

(setq custom-theme-directory themes-dir)

;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (mustang-netrom)))
 '(custom-safe-themes
   (quote
    ("c07ddfe89cbb24c8a0523c4f87ba9a7e82cd079fae9d43d6c0421501055666ba" default)))
 '(package-selected-packages
   (quote
    (ansi package-build shut-up epl git commander f dash s unfill comment-dwim-2 lsp-php el-get flycheck-pycheckers lsp-ui lsp-python company-lsp helm-xref cquery lsp-mode company-ghc hindent haskell-mode cask ert-runner diminish treemacs-projectile treemacs modern-cpp-font-lock hydra helm-flycheck zygospore windresize window-numbering vlf vkill swift-mode string-edit spaceline smartparens req-package rainbow-mode rainbow-delimiters php-mode on-screen nlinum multiple-cursors markdown-mode magit load-dir keyfreq key-chord json-mode indent-guide highlight-thing highlight-numbers highlight-escape-sequences highlight-current-line helm-swoop helm-projectile helm-ls-git helm-gtags helm-flx helm-c-yasnippet helm-ag golden-ratio-scroll-screen gitignore-mode gitconfig-mode git-timemachine git-messenger flyspell-lazy flycheck flx-ido fix-word find-temp-file fic-mode expand-region exec-path-from-shell edit-server dummy-h-mode dumb-jump discover-my-major dired-sort dired-narrow diff-hl describe-number dashboard dash-at-point csharp-mode copy-as-format company-statistics company-flx company-c-headers cmake-mode clang-format bury-successful-compilation avy auto-dim-other-buffers auto-dictionary auto-compile anzu ace-jump-mode ace-isearch))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Must come before any package configurations!
(package-initialize)

;; Prefer newest version of a file, especially for compiled files this is
;; useful.
(setq load-prefer-newer t)

;; Load general stuff that other init.d things might use.
(load functions-file)
(load general-file)

;; Create necessary directories if missing.
(mkdir user-cache-dir)

;; Speedup loading by removing handlers until finished. It contains a lot of regexps for matching
;; handlers to file names but it is not necessary while loading.
(setq file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Timing.
(setq initial-done-time (current-time))

(defun show-elapsed-time (msg start end)
  (let ((elapsed (float-time (time-subtract end start))))
    (message "%s %.3fs" msg elapsed)))

(defun show-loading-info ()
  (let ((cur (current-time)))
    (message "============================")
    (show-elapsed-time "Initial setup:  " emacs-start-time initial-done-time)
    (show-elapsed-time "Loaded packages:" initial-done-time cur)
    (show-elapsed-time "Total:          " emacs-start-time cur)
    (message "============================")))

;; Executed when loading is done.
(defun loading-done ()
  (show-loading-info)

  ;; Restore the file name handlers.
  (setq file-name-handler-alist file-name-handler-alist-old)

  ;; Start daemon server if not already running.
  (require 'server)
  (unless (server-running-p)
    (server-start))

  (byte-compile-confs-if-not-present))

;; Packages setup.
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ;("org" . "https://orgmode.org/elpa/")
                         ;("marmalade" . "https://marmalade-repo.org/packages/")
                         ))

(eval-when-compile (package-initialize))

(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

;; The "backbone" uses req-package.
(require-package 'use-package)
(require 'use-package)

(use-package req-package
  :ensure t
  :config
  (req-package--log-set-level 'debug))

(req-package auto-compile
  :ensure t
  :config
    (require 'auto-compile)
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode))

(random t)

(req-package load-dir
  :ensure t
  :force true
  :init
  (setq force-load-messages nil)
  (setq load-dir-debug nil)
  (setq load-dir-recursive t)
  :config
  (load-dir-one init-dir)
  (req-package-finish)
  (loading-done))
