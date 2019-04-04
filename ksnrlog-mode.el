;; Major mode to fontity KeyShot Network Rendering log files.

(defvar ksnrlog-highlights nil "Highlights of ksnrlog-mode.")

;; It will take the first regexp match so the order matters.
(setq ksnrlog-highlights
      '(;; Timestamps, like "Tue Jan 23 10:52:05 2018".
        ("\\w+ \\w+ [0-9]+ [0-9]+:[0-9]+:[0-9]+ [0-9]+" . font-lock-comment-face)

        ;; Strings.
        ("\\(\\\"\\|'\\).+\\(\\\"\\|'\\)" . font-lock-string-face)

        ;; Log levels.
        ;; Old logs look like "[EE]" while newer ones like "[EE default  ]".
        ;;("\\[\\(TT\\|DD\\|II\\)\\]?" . (1 font-lock-function-name-face))
        ("\\[\\(WW\\|EE\\|FF\\)\\]?" . (1 font-lock-warning-face))

        ;; Log categories.
        ("\\[\\w+ \\(default\\)\s*\\]" . (1 font-lock-function-name-face))
        ;; Make non-default categories stand out.
        ("\\[\\w+ \\(\\w+\\)\s*\\]" . (1 font-lock-type-face))

        ;; Delimiters.
        (",\\|:\\|;\\|-\\|(\\|)\\|\\[\\|\\]" . font-lock-builtin-face)

        ;; Numbers.
        ("\\(-\\)?[0-9.]+" . font-lock-constant-face)

        ;; Booleans.
        ("true\\|True\\|TRUE\\|false\\|False\\|FALSE" . font-lock-constant-face)))

(define-derived-mode ksnrlog-mode text-mode "ksnrlog"
  "major mode for viewing KeyShot Network Rendering log files."
  (setq font-lock-defaults '(ksnrlog-highlights)))
