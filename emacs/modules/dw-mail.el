(use-package gnus
  :ensure nil
  :config
  (setq gnus-select-method '(nnnil ""))
  (setq gnus-secondary-select-methods
        '((nnimap "imap.fastmail.com")
          (nntp "gwene" (nntp-address "news.gwene.org"))
          (nntp "news.yhetil.org")))

  (require 'gnus-demon)
  (gnus-demon-add-scanmail)
  (gnus-demon-init))
