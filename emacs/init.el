;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))
(package-initialize)

(require 'subr-x)
(setq dw/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

;; Fix an issue accessing the elpa archive
(when dw/is-termux
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Ensure latest Org with contrib is installed first
(use-package org :ensure org-plus-contrib)

(setq dw/exwm-enabled
      (and (not dw/is-termux)
           (eq window-system 'x)
           (seq-contains command-line-args "--use-exwm")))

;; Set up exwm early in the init process
(use-package exwm
  :ensure t
  :if dw/exwm-enabled
  :init
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  :config
  (require 'exwm-config)
  (exwm-config-default))

;; Load customization settings from another file
(setq custom-file "~/.emacs.d/config/customize.el")
(load custom-file)

;; Load real configuration from config.org
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
