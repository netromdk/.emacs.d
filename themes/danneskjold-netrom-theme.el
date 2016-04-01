;;; danneskjold-theme.el --- beautiful high-contrast theme

;; Copyright (c) 2016 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; URL: https://github.com/rails-to-cosmos/
;; Package-Version: 20160320.1512
;; Package-X-Original-Version: 20160311.458

;;; Commentary:

;;; Code:
(deftheme danneskjold-netrom "Port of danneskjold.")

(let ((bg "#202020")
      (fg "#e2e2e2")

      ;; Shades of grey
      (black "#000000")
      (frost "#D0E1F9" )
      (comment "#8593AE")
      (anthracite "#3a3f4b")
      (slightly-brighter-than-midnight "#282c34")
      (as-dark-as-midnight "#21252b")
      (white "#FFFFFF")

      ;; Shades of yellow
      (sunrise "#FFDB45")
      (saffron "#F9BA32")

      ;; Shades of green
      (spring-flower "#B3DE81")
      (summer-flower "#2f5218")

      ;; Shades of blue
      (twitter "#4CB5F5")
      (dark-twitter "#202060")

      ;; Shades of red
      (waddles "#FF87BA")
      (krayola "#E38B75")
      (santa "#F34A4A")
      (red-forest "#330006")
      (redder-forest "#aa0006"))

  (custom-theme-set-faces 'danneskjold-netrom
   `(default ((t (:foreground ,fg :background ,bg))))
   `(fringe ((t (:background ,bg))))
   `(region ((t (:background ,anthracite))))
   `(button ((t (:foreground ,frost :underline t))))
   `(link ((t (:foreground ,frost :underline t))))
   `(menu ((t (:foreground ,fg :background ,as-dark-as-midnight))))

   `(font-lock-string-face ((t (:foreground ,krayola))))
   `(font-lock-builtin-face ((t (:foreground ,twitter))))
   `(font-lock-variable-name-face ((t (:foreground ,sunrise))))
   `(font-lock-keyword-face ((t (:foreground ,frost))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:foreground ,saffron))))
   `(font-lock-constant-face ((t (:foreground ,fg))))

   `(mmm-default-submode-face ((t (:background ,as-dark-as-midnight))))

   ;; Mode-line
   `(mode-line ((t (:background ,as-dark-as-midnight
                    :foreground ,fg
                    :box ,(list
                           :line-width 4
                           :color as-dark-as-midnight)))))
   `(mode-line-inactive ((t (:background ,slightly-brighter-than-midnight
                             :foreground ,fg
                             :box ,(list
                                    :line-width 4
                                    :color slightly-brighter-than-midnight)))))
   `(mode-line-buffer-id ((t (:foreground ,fg))))

   `(font-lock-warning-face ((t (:foreground ,santa))))
   `(highlight ((t (:background ,as-dark-as-midnight :foreground ,frost))))

   `(linum ((t (:foreground ,anthracite))))

   `(widget-field ((t (:foreground ,fg :background ,slightly-brighter-than-midnight))))
   `(widget-button ((t (:foreground ,saffron))))

   ;; Highlight current line
   `(highlight-current-line-face ((t (:background ,slightly-brighter-than-midnight))))

   ;; Highlight numbers
   `(highlight-numbers-number ((t (:foreground ,saffron))))

   ;; Highlight quoted mode-line
   `(highlight-quoted-symbol ((t (:foreground ,waddles))))

   ;; Hl-line and hlinum-activate
   `(linum-highlight-face ((t (:foreground ,anthracite :background ,slightly-brighter-than-midnight :weight bold))))
   `(hl-line ((t (:background ,slightly-brighter-than-midnight))))

   ;; Magit
   `(magit-diff-added ((t (:background ,summer-flower :foreground ,fg))))
   `(magit-diff-added-highlight ((t (:background ,summer-flower :foreground ,fg))))
   `(magit-diff-removed ((t (:background ,red-forest :foreground ,fg))))
   `(magit-diff-removed-highlight ((t (:background ,redder-forest :foreground ,fg))))
   `(magit-diff-context ((t (:background ,bg :foreground ,comment))))
   `(magit-diff-context-highlight ((t (:background ,bg :foreground ,frost))))
   `(magit-section-highlight ((t (:background ,as-dark-as-midnight))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,bg))))
   `(magit-section-heading ((t (:foreground ,sunrise :inherit nil))))
   `(magit-diff-hunk-heading ((t (:foreground ,frost :background ,slightly-brighter-than-midnight))))
   `(magit-diff-lines-heading ((t (:foreground ,frost :background ,slightly-brighter-than-midnight))))

   ;; Diff-hl
   `(diff-hl-insert ((t (:inherit magit-diff-added :foreground ,spring-flower ))))
   `(diff-hl-change ((t (:background ,dark-twitter :foreground ,twitter))))
   `(diff-hl-delete ((t (:background ,red-forest :foreground ,santa))))

   ;; Org
   `(org-todo ((t (:foreground ,santa))))
   `(org-done ((t (:foreground ,spring-flower))))
   ;; `(org-date ((t (:foreground ,sunrise))))
   `(org-hide ((t (:foreground ,anthracite))))
   `(org-link ((t (:foreground ,frost :underline t))))
   `(org-date ((t (:foreground ,comment))))

   `(org-level-1 ((t (:foreground ,twitter))))
   `(org-level-2 ((t (:foreground ,frost))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,spring-flower))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,twitter))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,sunrise))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,waddles))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,saffron))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,sunrise))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,spring-flower))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,twitter))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,sunrise))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,waddles))))

   ;; Company-mode
   `(company-tooltip ((t (:foreground ,fg :background ,anthracite))))
   `(company-tooltip-selection ((t (:foreground ,fg :background ,slightly-brighter-than-midnight))))
   `(company-scrollbar-fg ((t (:background ,anthracite))))
   `(company-scrollbar-bg ((t (:background ,slightly-brighter-than-midnight))))
   `(company-tooltip-common ((t (:foreground ,saffron))))
   `(company-preview ((t (:background ,slightly-brighter-than-midnight))))
   `(company-preview-common ((t (:background ,slightly-brighter-than-midnight :foreground ,santa))))
   `(company-mouse ((t (:background ,as-dark-as-midnight))))

   ;; Flycheck
   `(flycheck-warning ((t (:foreground ,santa :underline t))))

   ;; Eshell
   `(eshell-prompt ((t (:foreground ,santa))))
   `(eshell-ls-executable ((t (:foreground ,spring-flower))))
   `(eshell-ls-directory ((t (:foreground ,twitter))))
   `(eshell-ls-readonly ((t (:foreground ,anthracite))))

   ;; Dired
   `(dired-directory ((t (:foreground ,twitter))))
   `(dired-git-face ((t (:foreground ,santa))))
   `(dired-ignored ((t (:foreground ,anthracite))))
   `(dired-filetype-omit ((t (:foreground ,anthracite))))
   `(dired-filetype-common ((t (:foreground ,saffron))))
   `(dired-filetype-execute ((t (:foreground ,spring-flower))))
   `(dired-filetype-source ((t (:foreground ,waddles))))
   `(dired-filetype-plain ((t (:foreground ,comment))))
   `(dired-filetype-link ((t (:foreground ,twitter :underline t))))
   `(dired-flagged ((t (:foreground ,santa :underline t))))
   `(dired-marked ((t (:foreground ,saffron :underline t))))
   `(diredp-dir-heading ((t (:foreground ,saffron))))
   `(dired-subtree-depth-1-face ((t (:background ,"#21252b"))))
   `(dired-subtree-depth-2-face ((t (:background ,"#282c34"))))
   `(dired-subtree-depth-3-face ((t (:background ,bg))))

   ;; Ido
   `(minibuffer-prompt ((t (:foreground ,comment))))
   `(ido-first-match ((t (:foreground ,frost))))
   `(ido-only-match ((t (:foreground ,frost))))
   `(ido-subdir ((t (:foreground ,frost))))
   `(ido-vertical-match-face ((t (:foreground ,twitter))))

   ;; Vertical-border
   `(vertical-border ((t (:foreground "#282a2e"))))

   ;; Cursor
   `(cursor ((t (:background ,spring-flower))))

   ;; Isearch
   `(isearch ((t (:bold t :foreground ,sunrise :background ,anthracite))))

   ;; Helm
   `(helm-selection ((t (:background ,slightly-brighter-than-midnight :foreground ,spring-flower :underline nil :weight bold))))
   `(helm-source-header ((t (:background ,as-dark-as-midnight  :foreground ,white :weight bold :height 1.3 :family "Sans Serif"))))
   `(helm-match ((t (:foreground ,saffron))))
   `(helm-buffer-directory ((t (:foreground ,comment)))) ;;808bed
   `(helm-ff-directory ((t (:inherit helm-buffer-directory))))
   `(helm-swoop-target-line-block-face ((t nil)))
   `(helm-swoop-target-line-face ((t (:background ,as-dark-as-midnight))))
   `(helm-swoop-target-word-face ((t (:background ,spring-flower :foreground ,black))))

   ;; Anzu
   `(anzu-mode-line ((t (:foreground ,spring-flower :weight bold))))
   `(anzu-replace-to ((t (:foreground ,saffron :slant italic :weight bold))))

   ;; On-screen-mode
   `(on-screen-fringe ((t (:foreground ,spring-flower))))

   ;; Whitespace
   `(whitespace-line ((t (:background "gray20" :foreground "#DD5542"))))

   ;; Highlight-thing
   `(hi-yellow ((t (:foreground ,spring-flower :weight bold :underline t))))

   ;; Compilation-mode
   `(compilation-warning ((t (:foreground ,sunrise))))
   `(compilation-info ((t (:foreground ,spring-flower))))
   `(compilation-error ((t (:inherit error :foreground ,santa))))
   `(compilation-line-number ((t (:inherit font-lock-keyword-face :foreground ,spring-flower))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'danneskjold-netrom)
;;; danneskjold-netrom-theme.el ends here
