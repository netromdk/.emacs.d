;;;;; kill-line
;;
;; Originally found at:
;; https://www.reddit.com/r/emacs/comments/rlli0u/whats_your_favorite_defadvice/

(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra
indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))
