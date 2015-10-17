(require 'req-package)

(req-package mu4e
  :require (org-mu4e ispell)
  :config
  (progn
    ;; Activate mode with keybinding that is bound by default to compose-mail.
    (global-set-key (kbd "C-x m") 'mu4e)

    ;; Don't ask to quit just return to previous buffer.
    (define-key mu4e-main-mode-map "q" 'previous-buffer)

    ;; Set as default MUA.
    (setq mail-user-agent 'mu4e-user-agent)

    (setq mu4e-maildir (expand-file-name "~/Maildir"))

    (setq mu4e-drafts-folder  "/[Gmail].Drafts")
    (setq mu4e-sent-folder    "/[Gmail].Sent Mail")
    (setq mu4e-trash-folder   "/[Gmail].Bin")
    (setq mu4e-refile-folder  "/[Gmail].All Mail")
    (setq mu4e-attachment-dir "~/Downloads")

    ;; Don't save message to Sent Messages, GMail/IMAP will take care of this.
    (setq mu4e-sent-messages-behavior 'delete)

    ;; Allow for updating mail using 'U' in the main view.
    (setq mu4e-get-mail-command "offlineimap")

    (setq mu4e-user-mail-address-list
          '("msk@nullpointer.dk"
            "ontherenth@gmail.com"
            "morten@luxion.com"))

    ;; Don't include addresses from mu4e-user-mail-address-list.
    (setq mu4e-compose-dont-reply-to-self t)

    ;; First time it will ask for user and pass, then save it to .authinfo
    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)

    ;; Add confirmation before sending emails.
    (add-hook 'message-send-hook
              (lambda ()
                (unless (yes-or-no-p "Sure you want to send this?")
                  (signal 'quit nil))))

    ;; Don't keep message buffers around after sending a message.
    (setq message-kill-buffer-on-exit t)

    ;; Skip duplicates when the Message-Id is the same (typically happens with
    ;; gmail with labels).
    (setq mu4e-headers-skip-duplicates t)

    ;; Number of headers to show while viewing an email.
    (setq mu4e-headers-visible-lines 20)

    ;; Use imagemagick, if available.
    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))

    ;; Define actions to view in browser with "ai".
    (add-to-list 'mu4e-headers-actions
                 '("in browser" . mu4e-action-view-in-browser) t)
    (add-to-list 'mu4e-view-actions
                 '("in browser" . mu4e-action-view-in-browser) t)

    ;; Remember to move the cursor to the header section before sending the
    ;; email for correct handling of org mode!
    (defalias 'org-mail 'org-mu4e-compose-org-mode)

    ;; Convert org mode to HTML automatically.
    (setq org-mu4e-convert-to-html t)

    ;; Need this to render HTML e-mails properly using w3m external program.
    (setq mu4e-html2text-command "w3m -dump -T text/html -O utf-8")

    ;; Some link navigation with tab and backtab.
    (add-hook 'mu4e-view-mode-hook
              (lambda()
                (local-set-key (kbd "<tab>") 'shr-next-link)
                (local-set-key (kbd "<backtab>") 'shr-previous-link)))

    ;; Prefer HTML versions (also so org-mode mails look good).
    (setq mu4e-view-prefer-html t)

    ;; Show addresses in addition to names.
    (setq mu4e-view-show-addresses t)

    ;; Show images inline.
    (setq mu4e-view-show-images t)

    ;; Show unicode characters.
    (setq mu4e-use-fancy-chars t)

    ;; Set appropriate time format.
    (setq mu4e-headers-time-format "%H:%M:%S")

    ;; Set appropriate date format.
    (setq mu4e-headers-date-format "%a %d/%m %Y")

    ;; Set the headers and their column sizes.
    (setq mu4e-headers-fields
          '((:human-date . 15) ;; Shows time for today and date for otherwise.
            (:flags . 6)
            (:size . 6)
            (:from-or-to . 22)
            (:subject . nil)))

    ;; Set the header fields to show when viewing emails.
    (setq mu4e-view-fields
          '(:from :to :cc :bcc :subject :flags :size :date :maildir
            :mailing-list :tags :attachments :signature :decryption))

    ;; Set bookmarks on the front.
    (setq mu4e-bookmarks
          '(("flag:unread AND NOT flag:trashed" "Unread messages" 117)
            ("date:today..now" "Today's messages" 116)
            ("flag:replied AND date:today..now" "Replied today" 114)
            ("date:7d..now" "Last 7 days" 119)
            ("flag:attach" "Messages with attachments" 97)
            ("mime:image/*" "Messages with images" 112)
            ("size:5M..500M" "Big messages" 98)
            ("html" "HTML messages" 104)
            ("text" "Text messages" 120)))

    ;; Do not auto-include a signature when composing mails.
    (setq mu4e-compose-signature-auto-include nil)

    ;; Insert newline before signature.
    (setq message-signature-insert-empty-line t)

    ;; Insert signature at point while preserving buffer position.
    (defun msk-mu4e-insert-signature-at-point()
      "Insert mail signature at point."
      (interactive)
      (save-excursion
        (when message-signature-insert-empty-line
          (insert "\n"))
        (insert "\n-- \n")
        (insert (eval mu4e-compose-signature))))

    ;; Override to insert at point instead of at the end.
    (define-key mu4e-compose-mode-map
      (kbd "C-c C-w") 'msk-mu4e-insert-signature-at-point)

    ;; Define each account.
    (defun msk-mu4e-msk()
      (interactive)
      (da-spell)
      (message "Personal account: msk@nullpointer.dk")
      (setq user-mail-address "msk@nullpointer.dk"
            user-full-name "Morten Kristensen"
            mu4e-compose-signature
            '(concat "Mvh. /Best regards\n"
                     "Morten Kristensen\n")
            mu4e-maildir-shortcuts
            '(("/INBOX" . ?i)
              ("/[Gmail].All Mail" . ?A)
              ("/[Gmail].Sent Mail" . ?S)
              ("/me/@msk" . ?m)
              ("/me.@ontherenth" . ?g)
              ("/me.e-boks" . ?e)
              ("/me.receipts" . ?r)
              ("/bujinkan.dojo" . ?d)
              ("/bujinkan.e-boks" . ?b)
              ("/bujinkan.kenkon" . ?k))))

    (defun msk-mu4e-ontherenth()
      (interactive)
      (da-spell)
      (message "Personal account: ontherenth@gmail.com")
      (setq user-mail-address "ontherenth@gmail.com"
            user-full-name "Morten Kristensen"
            mu4e-compose-signature
            '(concat "Mvh. /Best regards\n"
                     "Morten Kristensen\n")
            mu4e-maildir-shortcuts
            '(("/INBOX" . ?i)
              ("/[Gmail].All Mail" . ?A)
              ("/[Gmail].Sent Mail" . ?S)
              ("/me.@msk" . ?m)
              ("/me.@ontherenth" . ?g)
              ("/me.e-boks" . ?e)
              ("/me.receipts" . ?r)
              ("/bujinkan.dojo" . ?d)
              ("/bujinkan.e-boks" . ?b)
              ("/bujinkan.kenkon" . ?k))))

    (defun msk-mu4e-luxion()
      (interactive)
      (en-spell)
      (message "Work account: morten@luxion.com")
      (setq user-mail-address "morten@luxion.com"
            user-full-name "Morten Kristensen"
            mu4e-compose-signature
            '(concat "Mvh. /Best regards\n"
                     "Morten Kristensen\n\n"
                     "Software Engineer\n"
                     "Luxion ApS\n")
            mu4e-maildir-shortcuts
            '(("/INBOX" . ?i)
              ("/[Gmail].All Mail" . ?A)
              ("/[Gmail].Sent Mail" . ?S)
              ("/lux" . ?l)
              ("/lux.commits" . ?c)
              ("/lux.redmine" . ?r)
              ("/lux.velux" . ?v)
              ("/lux.@dev" . ?d)
              ("/lux.@aarhus" . ?å)
              ("/lux.@all" . ?a)
              ("/lux.@cloud" . ?u))))

    ;; Switch between active accounts.
    (define-key mu4e-main-mode-map (kbd "1") 'msk-mu4e-msk)
    (define-key mu4e-main-mode-map (kbd "2") 'msk-mu4e-ontherenth)
    (define-key mu4e-main-mode-map (kbd "3") 'msk-mu4e-luxion)
    (define-key mu4e-headers-mode-map (kbd "1") 'msk-mu4e-msk)
    (define-key mu4e-headers-mode-map (kbd "2") 'msk-mu4e-ontherenth)
    (define-key mu4e-headers-mode-map (kbd "3") 'msk-mu4e-luxion)

    ;; Check if addresses are used in to, cc or bcc fields.
    (defun msk-mu4e-is-message-to (msg rx)
      "Check if to, cc or bcc field in MSG has any address in RX."
      (or (mu4e-message-contact-field-matches msg :to rx)
          (mu4e-message-contact-field-matches msg :cc rx)
          (mu4e-message-contact-field-matches msg :bcc rx)))

    ;; Set replying identity from to, cc or bcc fields.
    (defun msk-mu4e-set-from-address ()
      "Set current identity based on to, cc, bcc of original."
      (let ((msg mu4e-compose-parent-message))
        (if msg
            (cond
             ((msk-mu4e-is-message-to msg (list "msk@nullpointer.dk"))
              (msk-mu4e-msk))
             ((msk-mu4e-is-message-to msg (list "ontherenth@gmail.com"))
              (msk-mu4e-ontherenth))
             ((msk-mu4e-is-message-to msg (list "morten@luxion.com"
                                                "cloud@luxion.com"
                                                "all@luxion.com"
                                                "dev@luxion.com"
                                                "aarhus@luxion.com"))
              (msk-mu4e-luxion))))))

    (add-hook 'mu4e-compose-pre-hook 'msk-mu4e-set-from-address)

    ;; When composing: enable flyspell, orgstruct++, orgtbl, auto-fill-mode,
    ;; visual-line-mode, and disable auto-saving so it doesn't create unwanted
    ;; mail files unless we want to save the draft. Do flyspell last so the
    ;; chosen dicitonary is shown in the message line.
    (add-hook 'mu4e-compose-mode-hook
              (lambda ()
                (auto-fill-mode 1)
                (visual-line-mode t)
                (orgstruct++-mode t)
                (orgtbl-mode t)
                (auto-save-mode -1)
                (flyspell-mode t)))

    ;; When viewing emails: turn on visual-line-mode.
    (add-hook 'mu4e-view-mode-hook
              (lambda ()
                (visual-line-mode t)))

    ;; Enable to compose an email with attachments from dired mode. Go into
    ;; dired somewhere, mark files to attach using 'm' and then do C-c RET C-a
    ;; to compose new email with those files as attachments.
    (require 'gnus-dired)
    ;; make the `gnus-dired-mail-buffers' function also work on
    ;; message-mode derived modes, such as mu4e-compose-mode
    (defun gnus-dired-mail-buffers()
      "Return a list of active message buffers."
      (let (buffers)
        (save-current-buffer
          (dolist (buffer (buffer-list t))
            (set-buffer buffer)
            (when (and (derived-mode-p 'message-mode)
                       (null message-sent-message-via))
              (push (buffer-name buffer) buffers))))
        (nreverse buffers)))

    (setq gnus-dired-mail-mode 'mu4e-user-agent)
    (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

    (setq message-citation-line-function 'message-insert-formatted-citation-line)
    (setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")

    ;; Choose default account.
    (msk-mu4e-msk)

    ;; Start mu4e in the background.
    (mu4e t)))

(req-package mu4e-maildirs-extension
  :require mu4e
  :config
  (progn
    (mu4e-maildirs-extension)))


(provide 'init-mu4e)
