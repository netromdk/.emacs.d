(require 'req-package)

(req-package dash-at-point
  :config
  (progn
    (global-set-key "\C-cd" 'dash-at-point)

    (add-to-list 'dash-at-point-mode-alist '(c-mode . "c,manpages"))
    (add-to-list 'dash-at-point-mode-alist '(c++-mode . "cpp,qt,c,manpages,lux"))
    (add-to-list 'dash-at-point-mode-alist '(python-mode . "py,flask"))
    (add-to-list 'dash-at-point-mode-alist '(cmake-mode . "cmake"))
    (add-to-list 'dash-at-point-mode-alist '(js-mode . "js"))))


(provide 'init-dash-at-point)
