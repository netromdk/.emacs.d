;;; init-flycheck --- Summary
;;; Commentary:
;;; Code:
(require 'req-package)

;; Disabled for now because it doesn't always work as intended and can be annoying..
;; (req-package flycheck
;;   :config
;;   (progn
;;     ;; C++11
;;     (add-hook 'c++-mode-hook
;;               (lambda ()
;;                 (progn
;;                   (setq flycheck-clang-language-standard "c++11")
;;                   (setq flycheck-clang-standard-library "libc++")
;;                   (setq flycheck-gcc-language-standard "c++11"))))

;;     (add-hook 'prog-mode-hook 'flycheck-mode)))


(provide 'init-flycheck)
;;; init-flycheck.el ends here
