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
    ("5d90c8e69ae1687d7b1dd4d8964695dbce3143e78e91425a8f1a9f01eccc9390" default)))
 '(package-selected-packages
   (quote
    (lsp-python company-lsp helm-xref cquery lsp-mode use-package use-package-el-get auto-compile load-dir req-package))))

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

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
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

(require-package 'use-package)
(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require-package 'el-get)
(require 'el-get)
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
(el-get 'sync)

(use-package use-package-el-get
  :ensure t
  :config (use-package-el-get-setup))

(use-package req-package
  :ensure t
  :config (req-package--log-set-level 'debug))

(use-package auto-compile
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
