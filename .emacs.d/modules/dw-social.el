;; -*- lexical-binding: t; -*-

(use-package tracking
  :demand t
  :config
  (setq tracking-faces-priorities '(all-the-icons-pink
                                    all-the-icons-lgreen
                                    all-the-icons-lblue))
  (setq tracking-frame-behavior nil
        tracking-shorten-buffer-names-p nil)
  (tracking-mode 1))

;; Add faces for specific people in the modeline.  There must
;; be a better way to do this.
(defun dw/around-tracking-add-buffer (original-func buffer &optional faces)
  (let* ((name (buffer-name buffer))
         (face (cond ((s-contains? "Maria" name) '(all-the-icons-pink))
                     ((s-contains? "Alex " name) '(all-the-icons-lgreen))
                     ((s-contains? "Steve" name) '(all-the-icons-lblue))))
         (result (apply original-func buffer (list face))))
    (when dw/exwm-enabled
      (dw/update-polybar-telegram))
    result))

(defun dw/after-tracking-remove-buffer (buffer)
  (when dw/exwm-enabled
    (dw/update-polybar-telegram)))

;;(advice-add 'tracking-add-buffer :around #'dw/around-tracking-add-buffer)
;;(advice-add 'tracking-remove-buffer :after #'dw/after-tracking-remove-buffer)
;;(advice-remove 'tracking-remove-buffer #'dw/around-tracking-remove-buffer)

;; Advise exwm-workspace-switch so that we can more reliably clear tracking buffers
;; NOTE: This is a hack and I hate it.  It'd be great to find a better solution.
(defun dw/before-exwm-workspace-switch (frame-or-index &optional force)
  (when (fboundp 'tracking-remove-visible-buffers)
    (when (eq exwm-workspace-current-index 0)
      (tracking-remove-visible-buffers))))

;;(advice-add 'exwm-workspace-switch :before #'dw/before-exwm-workspace-switch)

(use-package telega
  :commands telega
  :config
  (setq telega-use-tracking-for '(or unmuted mention)
        telega-completing-read-function #'completing-read
        telega-msg-rainbow-title t
        telega-chat-fill-column 75))

(unless (package-installed-p '0x0)
  (package-vc-install '(0x0 :url "https://gitlab.com/willvaughn/emacs-0x0.git")))

(use-package 0x0
  :commands (0x0-upload-file 0x0-upload-text))

(setq rcirc-server-alist '(("chat.sr.ht" :port 6697 :nick "daviwil" :user-name "daviwil" :encryption tls)
                           ("chat.sr.ht" :server-alias "libera.chat" :port 6697 :nick "daviwil" :user-name "daviwil/liberachat" :encryption tls)
                           ("chat.sr.ht" :server-alias "OFTC" :port 6697 :nick "daviwil" :user-name "daviwil/oftc" :encryption tls))
      rcirc-reconnect-delay 5
      rcirc-fill-column 120)

;; (use-package srv :ensure t)
;; (use-package fsm :ensure t)
;; In Guix
;; (setup (:pkg jabber ;;
;;              :host nil
;;              :repo "https://tildegit.org/wgreenhouse/emacs-jabber"))

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

(use-package elpher
  :commands elpher)

(use-package ement
  :commands ement-connect
  :custom
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

(provide 'dw-social)
