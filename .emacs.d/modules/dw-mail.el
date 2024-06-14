;; -*- lexical-binding: t; -*-

(use-package mu4e
  :ensure nil ; Installed via distro package manager
  ;;:defer 20 ; Wait until 20 seconds after startup
  :defer t
  :bind (("C-c m m" . mu4e)
         ("C-c m c" . 'mu4e-compose-new)
         ("C-c m i" . 'dw/go-to-inbox)
         ("C-c m s" . 'mu4e-update-mail-and-index))
  :hook ((mu4e-compose-mode . turn-off-auto-fill)
         (mu4e-compose-mode . turn-off-auto-fill)
         )
  :config
  ;; Refresh mail using isync every 10 minutes
  ;; NOTE: Trying to refresh mail manually now to increase focus
  ;;(setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/Mail")

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; Set up contexts for email accounts
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Fastmail"
            :vars '((user-full-name . "David Wilson")
                    (user-mail-address . "david@daviwil.com")
                    (mu4e-sent-folder . "/Fastmail/Sent Items")
                    (mu4e-trash-folder . "/Fastmail/Trash")
                    (mu4e-drafts-folder . "/Fastmail/Drafts")
                    (mu4e-refile-folder . "/Fastmail/Archive")
                    (mu4e-sent-messages-behavior . sent)))))
  (setq mu4e-context-policy 'pick-first)

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))

  (setf (alist-get 'trash mu4e-marks)
        '(:char ("d" . "▼")
                :prompt "dtrash"
                :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                :action (lambda (docid msg target)
                          (mu4e--server-move docid
                                             (mu4e--mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Use mu4e for sending e-mail
  (setq mail-user-agent 'mu4e-user-agent
        message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl)

  ;; Signing messages (use mml-secure-sign-pgpmime)
  (setq mml-secure-openpgp-signers '("3A74994A3EB078555A0FFDD599F6A2219E3A3C44"))

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  (setq mu4e-maildir-shortcuts
        '(("/Fastmail/INBOX" . ?i)
          ("/Fastmail/Archive" . ?a)
          ("/Fastmail/Lists/*" . ?l)
          ("/Fastmail/Sent Items" . ?s)
          ("/Fastmail/Trash" . ?t)))

  (add-to-list 'mu4e-bookmarks
               '(:name "All Inboxes"
                       :query "maildir:/Fastmail/INBOX"
                       :key ?i))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq dw/mu4e-inbox-query
        "(maildir:/Fastmail/INBOX) AND flag:unread")

  (defun dw/go-to-inbox ()
    (interactive)
    (mu4e-headers-search dw/mu4e-inbox-query))

  ;; Start mu4e in the background so that it syncs mail periodically
  (mu4e t))

(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query
        (concat dw/mu4e-inbox-query
                " date:30M..now"))

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  (mu4e-alert-enable-notifications))

(provide 'dw-mail)
