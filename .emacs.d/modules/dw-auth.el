;; -*- lexical-binding: t; -*-

;;; -- Use Emacs for Pinentry -----

(use-package pinentry
  :config
  (unless (or dw/is-termux
              (eq system-type 'windows-nt))
    (setq epa-pinentry-mode 'loopback)
    (pinentry-start)))

;;; -- Password Management -----

(use-package password-store
  :bind (("C-c p p" . password-store-copy)
         ("C-c p i" . password-store-insert)
         ("C-c p g" . password-store-generate))
  :config
  (setq password-store-password-length 12))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;;; -- OAuth2 -----

(use-package oauth2
  :ensure t)

(provide 'dw-auth)
