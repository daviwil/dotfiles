;; -*- lexical-binding: t; -*-

(setup (:pkg guix))

(dw/leader-key-def
  "G"  '(:ignore t :which-key "Guix")
  "Gg" '(guix :which-key "Guix")
  "Gi" '(guix-installed-user-packages :which-key "user packages")
  "GI" '(guix-installed-system-packages :which-key "system packages")
  "Gp" '(guix-packages-by-name :which-key "search packages")
  "GP" '(guix-pull :which-key "pull"))

(setup (:pkg daemons))

(setup (:pkg pulseaudio-control))

(defun dw/bluetooth-connect-q30 ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 11:14:00:00:1E:1A"))

(defun dw/bluetooth-connect-qc35 ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 04:52:C7:5E:5C:A8"))

(defun dw/bluetooth-disconnect ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- disconnect"))

(setup proced
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

(setup (:pkg docker))

(provide 'dw-system)
