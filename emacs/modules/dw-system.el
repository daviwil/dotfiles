;; -*- lexical-binding: t; -*-

(defun dw/guix-update-channels ()
  (interactive)
  (async-shell-command "~/.dotfiles/.files/.bin/update-channels" "*guix-channel-update*"))

(defun dw/guix-update-system ()
  (interactive)
  (async-shell-command "~/.dotfiles/.files/.bin/update-system" "*guix-system-update*"))

(use-package guix
  :bind (("C-c G p" . guix-packages-by-name)
         ("C-c G u c" . dw/guix-update-channels)
         ("C-c G u s" . dw/guix-update-system)))

(provide 'dw-system)
