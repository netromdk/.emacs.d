(require 'req-package)

(req-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.2)

  (key-chord-define-global "qq" 'kill-this-buffer)
  (key-chord-define-global "''" "`'\C-b")
  (key-chord-define-global ",," 'indent-for-comment-and-indent)

  (key-chord-mode 1))


(provide 'init-chords)
