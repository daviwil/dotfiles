;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Uncomment this to get a reading on packages that get loaded at startup
;;(setq use-package-verbose t)

(require 'subr-x)
(setq dw/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Fix an issue accessing the ELPA archive in Termux
(when dw/is-termux
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(unless package-archive-contents
  (package-refresh-contents))
(package-initialize)

;; Add my elisp path to load-path
(push "~/.emacs.d/elisp" load-path)

;; Are we running in Guix System?
;; TODO: Need a more accurate check for this!
(setq dw/is-guix-system (eq window-system 'x))

;; Initialize use-package on non-Linux platforms
(unless (or (package-installed-p 'use-package)
            dw/is-guix-system)
   (package-install 'use-package))
(require 'use-package)

;; On non-Guix systems, "ensure" packages by default
(setq use-package-always-ensure (not dw/is-guix-system))

;; Ensure latest Org with contrib is installed first
(when (not dw/is-guix-system)
  (use-package org :ensure org-plus-contrib))

(setq dw/exwm-enabled
      (and (not dw/is-termux)
           (eq window-system 'x)
           (seq-contains command-line-args "--use-exwm")))

;; Set up exwm early in the init process
(use-package exwm
  :if dw/exwm-enabled
  :init
  (setq mouse-autoselect-window t
        focus-follows-mouse t
        exwm-workspace-warp-cursor t
        exwm-workspace-number 5)
  :config
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (string-equal exwm-class-name "Vimb")
                (exwm-workspace-rename-buffer (format "Vimb: %s" exwm-title)))))

  (exwm-enable))

;; Enable exwm-randr before exwm-init gets called
(use-package exwm-randr
  :after (exwm)
  :config
  (exwm-randr-enable)
  (setq exwm-randr-workspace-monitor-plist '(1 "DP-1-2" 4 "eDP-1")))

;; Re-enable this after I get per-system defines implemented
(setq dw/mail-enabled (string-equal system-name "zerocool"))

;; Load real configuration from config.org
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

;; Keep transient cruft out of ~/.emacs.d/
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
