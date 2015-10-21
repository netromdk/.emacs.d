;; ------------------------
;;   Emacs Configurations
;; ------------------------

;; Constants.
(defconst init-dir "~/.emacs.d/init.d")
(defconst backup-dir (concat temporary-file-directory "emacs"))
(defconst yas-dir (concat user-emacs-directory "snippets"))
(make-directory backup-dir)

;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (mustang-netrom)))
 '(custom-safe-themes
   (quote
    ("f2e05195669230a1d91401a1c3eea679ee8a89f6237e7dba62ddef14518b90f9" default)))
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

;; Backups cleanup.
(message "Deleting backup files older than a week...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files backup-dir t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (nth 5 (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

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
