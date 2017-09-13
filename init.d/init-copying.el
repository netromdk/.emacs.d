(require 'req-package)

;; Convenience for formatting things to be copied and inserted elsewhere, like code blocks.
(req-package copy-as-format
  :require hydra
  :config

  (defhydra copying-hydra (:color blue :columns 5 :idle 1)
    "Copy as format"
    ("s" copy-as-format-slack "slack")
    ("j" copy-as-format-jira "jira")
    ("m" copy-as-format-markdown "markdown")
    ("g" copy-as-format-github "github")
    ("l" copy-as-format-gitlab "gitlab")
    ("h" copy-as-format-html "html")
    ("o" copy-as-format-org-mode "org-mode")
    ("w" copy-as-format-mediawiki "mediawiki")
    ("b" copy-as-format-bitbucket "bitbucket")
    ("r" copy-as-format-rst "rst")
    ("p" copy-as-format-pod "pod")
    ("c" copy-as-format-hipchat "hipchat")
    ("d" copy-as-format-disqus "discus"))

  (global-set-key (kbd "C-c w") 'copying-hydra/body))

(provide 'init-copying)
