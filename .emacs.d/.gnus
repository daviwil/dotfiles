(setq user-mail-address "david@daviwil.com")
(setq user-full-name "David Wilson")

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date))

(setq gnus-select-method
      '(nnimap "outlook"
	       (nnimap-address "imap-mail.outlook.com")
	       (nnimap-server-port 993)
	       (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp-mail.outlook.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp-mail.outlook.com" 587
				   "david@daviwil.com" nil))
      smtpmail-default-smtp-server "smtp-mail.outlook.com"
      smtpmail-smtp-server "smtp-mail.outlook.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
