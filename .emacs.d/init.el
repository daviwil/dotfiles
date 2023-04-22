;; -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'after-init-hook (lambda ()
                             ;; Make gc pauses faster by decreasing the threshold.
                             (setq gc-cons-threshold (* 2 1000 1000))))

;; Add configuration modules to load path
(add-to-list 'load-path '"~/.dotfiles/.emacs.d/modules")

;; Load pertinent modules
(require 'dw-package)
(require 'dw-settings)
(require 'dw-keys)
(require 'dw-keys-evil)
(require 'dw-core)
(require 'dw-interface)
(require 'dw-auth)
(require 'dw-shell)
(require 'dw-dev)
(require 'dw-dev-web)
(require 'dw-workflow)
(require 'dw-social)
(require 'dw-media)
(require 'dw-present)
(require 'dw-streaming)
(require 'dw-system)

(when (string= system-name "acidburn")
  (require 'dw-streaming))

(when dw/exwm-enabled (require 'dw-desktop))
(when dw/mail-enabled (require 'dw-mail))
