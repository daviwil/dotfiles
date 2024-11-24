;; -*- lexical-binding: t; -*-

;; Use `pass` as an auth-source
(when (file-exists-p "~/.password-store")
  (auth-source-pass-enable))

;; Enable GPG passphrase entry
(use-package pinentry
  :demand t
  :config
  (pinentry-start))

(provide 'dw-auth)
