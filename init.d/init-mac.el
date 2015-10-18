(when (eq system-type 'darwin)
  (progn
    (if (or (eq window-system 'ns)
            (eq window-system 'mac))
        (progn
          ;; avoid, e.g., hiding with M-h etc. (Carbon Emacs specific)
          ;;(setq mac-pass-command-to-system nil)

          ;; Let command be meta and alt be alt.
          (setq mac-option-key-is-meta nil)
          (setq mac-command-key-is-meta t)
          (setq mac-command-modifier 'meta)
          (setq mac-option-modifier nil)))

    (defun remove-dos-eol ()
      "Do not show ^M in files containing mixed UNIX and DOS line endings."
      (interactive)
      (setq buffer-display-table (make-display-table))
      (aset buffer-display-table ?\^M []))

    ;; ..and run it on all files.
    (add-hook 'find-file-hook 'remove-dos-eol)

    (defun open-with-finder ()
      "Show current buffer-file, or directory if in Dired-mode, in Finder."
      (interactive)
      (if (eq 'dired-mode major-mode)
          (shell-command "open .")
        (shell-command (concat "open -R '" (concat buffer-file-name "'")))))

    (defun toggle-fullscreen ()
      "Toggle full screen."
      (interactive)
      (set-frame-parameter
       nil 'fullscreen
       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))))


(provide 'init-mac)
