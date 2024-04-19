;; -*- lexical-binding: t; -*-

;; Add configuration modules to load path
(add-to-list 'load-path '"~/.dotfiles/.emacs.d/modules")


(defvar dw/fixed-pitch-font "JetBrains Mono"
  "The font used for `default' and `fixed-pitch' faces.")

(defvar dw/fixed-pitch-size 102)

(defvar dw/variable-pitch-font "Zen Kaku Gothic New"
  "The font used for `variable-pitch' face.")

(defvar dw/variable-pitch-size 120)

(defvar dw/org-heading-font "Alatsi"
  "The font used for Org Mode headings.")


;;; -- System Identification -----

(defvar dw/is-termux
  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(defvar dw/current-distro (or (and (eq system-type 'gnu/linux)
                                   (file-exists-p "/etc/os-release")
                                   (with-temp-buffer
                                     (insert-file-contents "/etc/os-release")
                                     (search-forward-regexp "^ID=\"?\\(.*\\)\"?$")
                                     (intern (or (match-string 1)
                                                 "unknown"))))
                              'unknown))

(defvar dw/is-guix-system (eql dw/current-distro 'guix))

(defvar dw/exwm-enabled (and (not dw/is-termux)
                             (eq window-system 'x)
                             (seq-contains command-line-args "--use-exwm")))

;; Load pertinent modules
(require 'dw-package)
(require 'dw-settings)
(require 'dw-keys)

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
(require 'dw-system)

(when (string= system-name "acidburn")
  (require 'dw-streaming)
  (setq dw/mail-enabled t))

(when dw/exwm-enabled (require 'dw-desktop))
(when dw/mail-enabled (require 'dw-mail))
