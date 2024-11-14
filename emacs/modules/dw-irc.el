;; -*- lexical-binding: t; -*-

(use-package rcirc
  :ensure nil
  :custom
  (rcirc-default-nick "daviwil")
  (rcirc-default-user-name "daviwil")
  (rcirc-default-full-name "David Wilson")
  (rcirc-server-alist `(("chat.sr.ht"
                         :port 6697
                         :encryption tls
                         :user-name "daviwil/irc.libera.chat@emacs")))

  (rcirc-reconnect-delay 5)
  (rcirc-fill-column 90)
  (rcirc-track-ignore-server-buffer-flag t)

  :config
  ;; Annoy me, please
  (rcirc-track-minor-mode 1)

  ;; See: https://idiomdrottning.org/rcirc-soju
  (defun-rcirc-command detach (channel)
    "Detach channel to soju."
    (interactive "sPart channel: ")
    (let ((channel (if (> (length channel) 0) channel target)))
      (rcirc-send-privmsg
       process "BouncerServ"
       (format
        "channel update %s -detached true -reattach-on highlight" channel)))))

(provide 'dw-irc)
