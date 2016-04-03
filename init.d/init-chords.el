(require 'req-package)

(req-package key-chord
  :config
  (key-chord-define-global "qw" 'kill-this-buffer)
  (key-chord-define-global "qq" 'kill-this-buffer)
  (key-chord-define-global "''" "`'\C-b")
  (key-chord-define-global ",," 'indent-for-comment-and-indent)

  (key-chord-mode 1))


(provide 'init-chords)
