(use-package gnus
  :ensure nil
  :config
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nnimap "fastmail"
           (nnimap-address "imap.fastmail.com")
           (nnir-search-engine imap)

           ;; Move expired mail items to the trash immediately
           (nnmail-expiry-target "nnimap+fastmail:Trash")
           (nnmail-expiry-wait immediate))
          (nntp "gwene" (nntp-address "news.gwene.org"))
          (nntp "news.yhetil.org")))

  (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications)

  (require 'gnus-demon)
  (setq gnus-demon-handlers
        '((gnus-demon-scan-news t 60)
          (gnus-demon-scan-mail t 60)))
  (gnus-demon-init))

(provide 'dw-mail)
