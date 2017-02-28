;;; Emacs Configurations                              -*- no-byte-compile: t -*-

(let ((major 25)
      (minor 1))
  (unless (and (>= emacs-major-version major)
               (>= emacs-minor-version minor))
    (error "Emacs v. %d.%d+ is required for this configuration!" major minor)))

;; Constants.
(defconst emacs-start-time (current-time))
(defconst init-dir (concat user-emacs-directory "init.d"))
(defconst backup-dir (concat temporary-file-directory "emacs"))
(defconst yas-dir (concat user-emacs-directory "snippets"))
(defconst themes-dir (concat user-emacs-directory "themes"))

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
    ("d179397d45e30bfb8e80f14156acafc74b79c241eea9b95846c1a4338dfa410a" default)))
 '(package-selected-packages
   (quote
    (company-statistics company-flx indent-guide async exec-path-from-shell esup edit-server copy-as-format highlight-escape-sequences ace-isearch bury-successful-compilation zygospore windresize window-numbering vlf vkill vim-empty-lines-mode swift-mode string-edit spaceline smartparens req-package rainbow-mode rainbow-delimiters php-mode package-safe-delete on-screen nlinum multiple-cursors markdown-mode magit load-dir keyfreq key-chord json-mode highlight-thing highlight-numbers highlight-current-line helm-swoop helm-projectile helm-ls-git helm-gtags helm-flx helm-c-yasnippet helm-ag golden-ratio-scroll-screen gitignore-mode gitconfig-mode git-timemachine git-messenger flyspell-lazy flycheck flx-ido find-temp-file fic-mode expand-region dummy-h-mode discover-my-major dired-sort dired-narrow diff-hl describe-number dash-at-point company-c-headers cmake-mode clang-format avy auto-dictionary auto-compile anzu))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Prefer newest version of a file, especially for compiled files this is
;; useful.
(setq load-prefer-newer t)

;; Load general stuff that other init.d things might use.
(load functions-file)
(load general-file)

;; Speedup loading by removing handlers until finished. It contains a lot of regexps for matching
;; handlers to file names but it is not necessary while loading.
(setq file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Packages setup.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(eval-when-compile (package-initialize))

(defun require-package (package)
  "Refresh package archives, check package presence and install if it's not installed."
  (if (null (require package nil t))
      (progn (let* ((ARCHIVES (if (null package-archive-contents)
                                  (progn (package-refresh-contents)
                                         package-archive-contents)
                                package-archive-contents))
                    (AVAIL (assoc package ARCHIVES)))
               (if AVAIL
                   (package-install package)))
             (require package))))

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

;; First install the auto-compile package and enable it.
(require-package 'auto-compile)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; The "backbone" uses req-package.
(require-package 'req-package)
(require 'req-package)
(random t)

(req-package load-dir
  :force t ; Load immediately!
  :defer 1
  :init
  (setq force-load-messages nil)
  (setq load-dir-debug nil)
  (setq load-dir-recursive t)
  :config
  (load-dir-one init-dir)
  (req-package-finish)
  (loading-done))
