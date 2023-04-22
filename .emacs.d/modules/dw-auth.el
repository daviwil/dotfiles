;; -*- lexical-binding: t; -*-

;;; -- Use Emacs for Pinentry -----

(unless (or dw/is-termux
            (eq system-type 'windows-nt))
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;;; -- Password Management -----

(use-package password-store
  :config
  (setq password-store-password-length 12)
  (define-key* dw/password-prefix-map
    "p" 'password-store-copy
    "i" 'password-store-insert
    "g" 'password-store-generate))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;;; -- OAuth2 -----

(use-package oauth2
  :ensure t)

(provide 'dw-auth)
