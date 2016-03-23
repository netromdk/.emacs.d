(require 'req-package)

(req-package diminish
  :config
  (progn
    (eval-after-load "anzu"
      '(diminish 'anzu-mode))

    (eval-after-load "vim-empty-lines-mode"
      '(diminish 'vim-empty-lines-mode))

    (eval-after-load "abbrev"
      '(diminish 'abbrev-mode "Abv"))

    (eval-after-load "fic-mode"
      '(diminish 'fic-mode))

    (eval-after-load "company"
      '(diminish 'company-mode "Comp"))

    (eval-after-load "smartparens"
      '(diminish 'smartparens-mode))

    (eval-after-load "org-table"
      '(diminish 'orgtbl-mode "OrgT"))

    (eval-after-load "org"
      '(diminish 'orgstruct-mode "OrgS"))

    (eval-after-load "rainbow-mode"
      '(diminish 'rainbow-mode))

    (eval-after-load "helm-gtags"
      '(diminish 'helm-gtags-mode "HGt"))

    (eval-after-load "yasnippet"
      '(diminish 'yas-minor-mode))

    (eval-after-load "highlight-thing"
      '(diminish 'highlight-thing-mode))

    (eval-after-load "hi-lock"
      '(diminish 'hi-lock-mode))))


(provide 'init-diminish)
