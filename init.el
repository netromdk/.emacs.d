;; ------------------------
;;   Emacs Configurations
;; ------------------------

;; Constants.
(defconst init-dir "~/.emacs.d/init.d")

;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (mustang-netrom)))
 '(custom-safe-themes
   (quote
    ("980f08fa0417c3a9cc5ada212bdf5de7b16fc3b499295d100a2277ce46f2e2a2" default)))
 '(recentf-exclude
   (quote
    ("ido.last" ".emacs.d/saveplace.txt" "/var/folders/" "~/Maildir")))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Recompile all configurations when closing emacs.
(add-hook 'kill-emacs-hook
          (lambda ()
            (byte-recompile-directory init-dir 0 t)))

;; Packages setup.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; Aliases for viewing packages.
(defalias 'lp 'package-list-packages)
(defalias 'lpn 'package-list-packages-no-fetch)

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

;; The "backbone" uses req-package.
(require-package 'req-package)
(require 'req-package)
(req-package-force load-dir
  :defer 1
  :init (progn (setq force-load-messages nil)
               (setq load-dir-debug nil)
               (setq load-dir-recursive t))
  :config (progn (load-dir-one init-dir)
                 (req-package-finish)))
