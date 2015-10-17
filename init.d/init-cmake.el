(require 'req-package)

(req-package cmake-mode
  :config
  (progn
    (setq auto-mode-alist
          (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode))
                  auto-mode-alist))))


(provide 'init-cmake)
