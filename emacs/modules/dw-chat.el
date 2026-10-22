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

(use-package jabber
  :custom
  (jabber-chat-buffer-format "Jabber: %n")
  :config
  ;; Unset the global key binding
  (global-set-key (kbd "C-x C-j") #'dired-jump))

;; IRCv3 client, connected to my soju bouncer on 0x11.run (wg0 only).
;; The network is named after the pass entry IRC/soju-0x11.run so that
;; auth-source-pass finds the bouncer password.
(use-package clatter
  :vc (:url "https://github.com/parenworks/clatter.el"
       :rev :newest)
  :custom
  (clatter-networks
   '(("soju-0x11.run"
      :server "10.11.0.1"
      :port 6698
      :tls nil
      :nick "daviwil"
      :username "daviwil"
      :bouncer t)))
  :config
  (clatter-setup))

(provide 'dw-chat)
