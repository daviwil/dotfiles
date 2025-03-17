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

;; -*- lexical-binding: t; -*-
(use-package xterm-color
  :demand t
  :config
  ;; Enable ANSI escape handling in eshell
  (with-eval-after-load 'eshell
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))

    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
    (setenv "TERM" "xterm-256color"))

  ;; Enable ANSI escape handling in M-x compile
  (setq compilation-environment '("TERM=xterm-256color"))

  (defun dw/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'dw/advice-compilation-filter))

(use-package eat
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode))
