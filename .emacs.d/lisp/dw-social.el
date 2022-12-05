;; -*- lexical-binding: t; -*-

(setup (:pkg tracking)
  (require 'tracking)
  (setq tracking-faces-priorities '(all-the-icons-pink
                                    all-the-icons-lgreen
                                    all-the-icons-lblue))
  (setq tracking-frame-behavior nil))

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

(setup (:pkg telega)
  (setq telega-use-tracking-for nil
        telega-completing-read-function #'completing-read
        telega-msg-rainbow-title t
        telega-chat-fill-column 75))

(dw/ctrl-c-keys
  "c"  '(:ignore t :which-key "chat")
  "cb" 'erc-switch-to-buffer
  "cc" 'dw/connect-irc
  "ca" 'erc-track-switch-buffer)

(setup (:pkg 0x0 :host gitlab :repo "willvaughn/emacs-0x0"))

(setq rcirc-server-alist '(("crafter.mx" :port 3110 :nick "daviwil" :encryption tls))
      rcirc-reconnect-delay 5
      rcirc-fill-column 120)

(add-hook 'rcirc-mode-hook #'evil-normal-state)

(setup (:pkg srv :straight t))
(setup (:pkg fsm :straight t)) ;; In Guix
(setup (:pkg jabber
             :host nil
             :repo "https://tildegit.org/wgreenhouse/emacs-jabber"))

(setup (:pkg mastodon :straight t)
  (setq mastodon-instance-url "https://fosstodon.org"
        mastodon-active-user "daviwil"))

(setup (:pkg elfeed)
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

(setup (:pkg elpher))

(provide 'dw-social)
