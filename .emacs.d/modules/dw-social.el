;; -*- lexical-binding: t; -*-

(use-package tracking)

(use-package telega
  :commands telega
  :config
  (setq telega-use-tracking-for '(or unmuted mention)
        telega-completing-read-function #'completing-read
        telega-msg-rainbow-title t
        telega-chat-fill-column 75)

  (when (eq dw/current-distro 'void)
    (setq telega-server-libs-prefix "/usr")))

(unless (package-installed-p '0x0)
  (package-vc-install '(0x0 :url "https://git.sr.ht/~willvaughn/emacs-0x0")))

(use-package 0x0
  :commands (0x0-upload-file 0x0-upload-text))

(use-package rcirc
  :ensure nil
  :custom
  (rcirc-default-nick "daviwil")
  (rcirc-default-user-name "daviwil")
  (rcirc-default-full-name "David Wilson")
  (rcirc-default-quit-reason "Hey, who put that C-x C-c there?")
  (rcirc-default-part-reason "Probably killed the channel buffer by accident...")
  (rcirc-server-alist `(("chat.sr.ht"
                         :port 6697
                         :encryption tls
                         :user-name "daviwil/irc.libera.chat@emacs"
                         :password ,(password-store-get "IRC/chat.sr.ht"))))
                                        ;'(("irc.libera.chat" :port 6697 :nick "daviwil" :user-name "daviwil" :encryption tls))
  (rcirc-reconnect-delay 5)
  (rcirc-fill-column 120)
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

(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://fosstodon.org"
        mastodon-active-user "daviwil"))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
        '("http://ruzkuku.com/all.atom"
          "https://systemcrafters.net/rss/news.xml"
          "https://codeberg.org/SystemCrafters/crafted-guix.rss"
          "https://karthinks.com/index.xml"
          "https://shom.dev/index.xml"
          "https://blog.benoitj.ca/posts/index.xml"
          "https://protesilaos.com/codelog.xml"
          "https://sachachua.com/blog/feed/"
          "https://nullprogram.com/feed/"
          "https://irreal.org/blog/?feed=rss2"
          "https://ambrevar.xyz/atom.xml"
          "https://guix.gnu.org/feeds/blog.atom"
          "https://valdyas.org/fading/feed/"
          "https://www.reddit.com/r/emacs/.rss")))

(use-package ement
  :commands ement-connect
  :custom
  (ement-notify-dbus-p nil) ;; Turn off notifications
  (ement-sessions-file "~/.cache/ement.el")
  (ement-room-list-default-keys
   '(;; Group all invitations (this group will appear first since the rooms are
     ;; already sorted first).
     ((membership :status 'invite))

     ;; Group all left rooms (this group will appear last, because the rooms are already
     ;; sorted last).
     ((membership :status 'leave))

     ;; Group all favorite rooms, which are already sorted first.
     (favourite)

     ;; Group all low-priority rooms, which are already sorted last.
     (low-priority)

     ;; Group other rooms which are unread.
     (unread)
     (people)
     freshness)))

(use-package newsticker
  :ensure nil
  :defer 5
  :config
  (setq newsticker-ticker-period 360
        newsticker-scroll-smoothly nil
        newsticker-ticker-interval 5
        newsticker-url-list
        '(("Emacs News" "https://sachachua.com/blog/feed/")
          ("David Thompson" "https://dthompson.us/feed.xml")
          ("Spritely Institute" "https://spritely.institute/feed.xml")
          ("guix-devel" "https://yhetil.org/guix-devel/new.atom")
          ("help-guix" "https://yhetil.org/guix-user/new.atom")
          ("guile-devel" "https://yhetil.org/guile-devel/new.atom")
          ("emacs-devel" "https://yhetil.org/emacs-devel/new.atom"))))
                                        ;(newsticker-start t)
                                        ;(newsticker-start-ticker))

(provide 'dw-social)
