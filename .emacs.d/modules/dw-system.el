;; -*- lexical-binding: t; -*-

(use-package guix)

(define-key* dw/guix-prefix-map
  "g" 'guix
  "i" 'guix-installed-user-packages
  "I" 'guix-installed-system-packages
  "p" 'guix-packages-by-name
  "P" 'guix-pull)

(use-package daemons)

(use-package pulseaudio-control)

(defun dw/bluetooth-connect-q30 ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 11:14:00:00:1E:1A"))

(defun dw/bluetooth-connect-qc35 ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 04:52:C7:5E:5C:A8"))

(defun dw/bluetooth-disconnect ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- disconnect"))

(use-package proced
  :config
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

(use-package docker
  :commands docker)

(provide 'dw-system)
