(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(c-basic-offset (quote set-from-style))
 '(custom-safe-themes
   (quote
	("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(erc-modules
   (quote
	(autoaway autojoin button completion fill irccontrols keep-place list match menu move-to-prompt netsplit networks noncommands notify notifications readonly ring smiley stamp track)))
 '(evil-collection-outline-bind-tab-p nil)
 '(global-magit-file-mode t)
 '(org-agenda-files
   (quote
	("c:/cygwin64/home/daviwil/Notes/Career.org" "c:/cygwin64/home/daviwil/Notes/Checklists.org" "c:/cygwin64/home/daviwil/Notes/Fitness.org" "c:/cygwin64/home/daviwil/Notes/Greek.org" "c:/cygwin64/home/daviwil/Notes/Guitar.org" "c:/cygwin64/home/daviwil/Notes/Habits.org" "c:/cygwin64/home/daviwil/Notes/Inbox.org" "c:/cygwin64/home/daviwil/Notes/Keyboard.org" "c:/cygwin64/home/daviwil/Notes/Metrics.org" "c:/cygwin64/home/daviwil/Notes/Mission.org" "c:/cygwin64/home/daviwil/Notes/MusicComposition.org" "c:/cygwin64/home/daviwil/Notes/MusicProduction.org" "c:/cygwin64/home/daviwil/Notes/Nutrition.org" "c:/cygwin64/home/daviwil/Notes/Personal.org" "c:/cygwin64/home/daviwil/Notes/Singing.org" "c:/cygwin64/home/daviwil/Notes/SoftwareDev.org" "c:/cygwin64/home/daviwil/Notes/Tools.org" "c:/cygwin64/home/daviwil/Notes/WindSynth.org" "c:/cygwin64/home/daviwil/Notes/Wisdom.org" "c:/cygwin64/home/daviwil/Notes/Work.org" "c:/cygwin64/home/daviwil/Notes/Workflow.org" "c:/cygwin64/home/daviwil/Notes/Writing.org")))
 '(org-agenda-hide-tags-regexp "agenda\\|process")
 '(org-agenda-prefix-format
   (quote
	((agenda . " %i %-18:c%?-12t% s")
	 (timeline . "  % s")
	 (todo . " %i %-18:c")
	 (tags . " %i %-18:c")
	 (search . " %i %-18:c"))))
 '(org-agenda-tags-column -120)
 '(org-cycle-separator-lines 1)
 '(org-ellipsis " ->")
 '(org-hide-block-startup t)
 '(org-highlight-sparse-tree-matches nil)
 '(org-pomodoro-long-break-frequency 10)
 '(org-startup-folded (quote content))
 '(package-selected-packages
   (quote
	(helm-projectile expand-region system-packages helm-pass buffer-flip use-package-chords mastodon counsel-spotify ivy-pass persp-mode erc-image erc-hl-nicks calfw-org calfw org-wild-notifier org-alert elfeed md4rd org-gcal eshell-prompt-extras mu4e-alert default-text-scale offlineimap notmuch dired-rainbow ghub+ org evil-escape wttrin circe-notifications nvm lsp-javascript-typescript js2-mode emms-player-mpv magit-gh-pulls sauron erc-status-sidebar dashboard eyebrowse lsp-ui f multi-term color-theme-sanityinc-tomorrow noctilux-theme nord-theme dakrone-theme smex evil-collection counsel-projectile cargo lsp-rust rust-mode avy tide ranger diminish org-bullets all-the-icons-ivy counsel ivy slack emojify general which-key twittering-mode emms restclient projectile smart-mode-line jabber nix-mode csharp-mode yasnippet impatient-mode web-mode markdown-mode fsharp-mode git-annex magit-annex git-gutter magit cider clojure-mode helm-company company ido-vertical-mode flx-ido rainbow-delimiters smartparens helm-themes helm-package helm deft org-journal org-pomodoro evil-nerd-commenter evil badger-theme use-package)))
 '(tab-width 4)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#bf616a")
	 (40 . "#DCA432")
	 (60 . "#ebcb8b")
	 (80 . "#B4EB89")
	 (100 . "#89EBCA")
	 (120 . "#89AAEB")
	 (140 . "#C189EB")
	 (160 . "#bf616a")
	 (180 . "#DCA432")
	 (200 . "#ebcb8b")
	 (220 . "#B4EB89")
	 (240 . "#89EBCA")
	 (260 . "#89AAEB")
	 (280 . "#C189EB")
	 (300 . "#bf616a")
	 (320 . "#DCA432")
	 (340 . "#ebcb8b")
	 (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil))

; '(default ((t (:foreground "#F6F3E8" :background "#171717"))))
; '(org-level-1 ((t (:background "gray13" :foreground "#8AC6F2" :height 1.2)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(circe-highlight-nick-face ((t (:foreground "dark orange" :weight bold))))
 '(erc-input-face ((t (:foreground "light steel blue"))))
 '(erc-my-nick-face ((t (:foreground "dodger blue" :weight bold))))
 '(erc-timestamp-face ((t (:foreground "tomato" :weight bold))))
 '(jabber-activity-face ((t (:foreground "brown2" :weight bold))))
 '(jabber-activity-personal-face ((t (:foreground "orange red" :weight bold))))
 '(jabber-chat-prompt-foreign ((t (:foreground "medium slate blue" :weight bold))))
 '(jabber-chat-prompt-local ((t (:foreground "green yellow" :weight bold))))
 '(jabber-roster-user-online ((t (:foreground "yellow green" :slant normal :weight bold))))
 '(lui-track-bar ((t (:inherit default :background "light gray" :foreground "orange red" :height 0.1)))))
