;; Shows the number of matches for searches.
(require 'req-package)

(req-package anzu
  :config
  (progn
    (defalias 'qrr 'anzu-query-replace-regexp)

    ;; Don't add to modeline because spaceline will show anzu.
    (setq anzu-cons-mode-line-p nil)

    ;; Deactivate region, if any, when using anzu replace functionality because it's
    ;; hard to see the search results with an active region as well.
    (setq anzu-deactivate-region t)

    ;; Change the mode-line text summary of search/replace results.
    (defun msk-anzu-update-func (here total)
      (when anzu--state
        (let ((status (cl-case anzu--state
                        (search (format "%d/%d" here total))
                        (replace-query (format "%d replaces" total))
                        (replace (format "%d/%d" here total)))))
          (propertize status 'face 'anzu-mode-line))))
    (setq anzu-mode-line-update-function #'msk-anzu-update-func)

    (global-anzu-mode t)))


(provide 'init-anzu)
