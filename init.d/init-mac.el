(if (or (eq window-system 'ns)
        (eq window-system 'mac))
    (progn
      ;; avoid, e.g., hiding with M-h etc. (Carbon Emacs specific)
      ;(setq mac-pass-command-to-system nil)

      ;; Let command be meta and alt be alt.
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))


(provide 'init-mac)
