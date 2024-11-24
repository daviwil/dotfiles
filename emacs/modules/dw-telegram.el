;; -*- lexical-binding: t; -*-

(use-package tracking
  :demand t)

(use-package telega
  :commands telega
  :config
  (setq telega-use-tracking-for '(or unmuted mention)
        telega-completing-read-function #'completing-read
        telega-msg-rainbow-title t
        telega-chat-fill-column 75)

  ;; Show notifications in the mode line
  (add-hook 'telega-load-hook #'telega-mode-line-hook)

  ;; Disable chat buffer auto-fill
  (add-hook 'telega-chat-mode-hook #'telega-chat-auto-fill-mode)

  (when (eq dw/current-distro 'void)
    (setq telega-server-libs-prefix "/usr")))

(provide 'dw-telegram)
