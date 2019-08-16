;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))
(package-initialize)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Ensure latest Org with contrib is installed first
(use-package org :ensure org-plus-contrib)

(setq dw/exwm-enabled
      (and (eq window-system 'x)
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
