;;; -*- lexical-binding: t; -*-

;; Prevent package.el loading packages prior to their init-file loading.
(setq package-enable-at-startup nil)

;; Speed up loading by disabling garbage collection and removing file name handlers until
;; finished. The handlers hold a lot of regexps that are not needed while loading.
(defvar --file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil
      gc-cons-threshold most-positive-fixnum)

;; Restore both in `emacs-startup-hook', which also runs when init.el signals an error. Handlers
;; added during init are kept. Each GC pauses Emacs for about 50 ms, so collect only at every 100
;; MB, and also after 10s without input, so pauses rarely hit typing.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist (delete-dups (append file-name-handler-alist
                                                               --file-name-handler-alist-old))
                  gc-cons-threshold (* 100 1024 1024))
            (run-with-idle-timer 10 t #'garbage-collect)))
