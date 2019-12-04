;; This file contains the list of packages that should be installed for
;; daily use after the base Guix system is online.

(specifications->manifest
 '(
   ;; The One
   "emacs"
   "emacs-use-package"
   "emacs-exwm"
   "emacs-evil"
   "emacs-evil-magit"
   "emacs-evil-collection"
   "emacs-evil-nerd-commenter"
   "emacs-general"
   "emacs-magit"
   "emacs-magit-todos"
   "emacs-forge"
   "emacs-hydra"
   "emacs-avy"
   "emacs-ivy"
   "emacs-ivy-xref"
   "emacs-ivy-pass"
   "emacs-ivy-rich"
   "emacs-flx"
   "emacs-smex"
   "emacs-counsel-projectile"
   "emacs-pinentry"
   "emacs-parinfer-mode"
   "emacs-default-text-scale"
   "emacs-expand-region"
   "emacs-dired-hacks"
   "emacs-org"
   "emacs-org-contrib"
   "emacs-org-bullets"
   "emacs-org-pomodoro"
   "emacs-git-link"
   "emacs-git-gutter"
   "emacs-projectile"
   "emacs-lsp-mode"
   "emacs-lsp-ui"
   "emacs-cider"
   "emacs-typescript-mode"
   "emacs-js2-mode"
   "emacs-prettier"
   "emacs-desktop-environment"
   "emacs-rust-mode"
   "emacs-helpful"
   "emacs-markdown-mode"
   "emacs-web-mode"
   "emacs-yaml-mode"
   "emacs-flycheck"
   "emacs-yasnippet"
   "emacs-ivy-yasnippet"  ;; Not in config yet
   "emacs-yasnippet-snippets"
   "emacs-smartparens"
   "emacs-rainbow-delimiters"
   "emacs-know-your-http-well"
   "emacs-darkroom"
   "emacs-mu4e-alert"
   "emacs-calfw"
   "emacs-eshell-z"
   "emacs-esh-autosuggest"
   "emacs-fish-completion"
   "emacs-multi-term"
   "emacs-xterm-color"
   "emacs-multiple-cursors" ;; Not in config yet
   "emacs-erc-image"
   "emacs-erc-hl-nicks"
   "emacs-mastodon"
   "emacs-elfeed"
   "emacs-emms"
   "emacs-alert"
   "emacs-guix"
   "emacs-daemons"
   "emacs-pulseaudio-control"
   "emacs-symon"
   "emacs-docker"
   "emacs-docker-tramp"
   "emacs-dockerfile-mode"
   "emacs-emojify"
   "emacs-minions"
   "emacs-spacegray-theme"
   "emacs-which-key"
   "emacs-exec-path-from-shell"
   "emacs-all-the-icons"
   "emacs-all-the-icons-dired"
   "emacs-tracking"
   "emacs-telega"
   "emacs-doom-modeline"

   ;; Developer tools
   "node"
   "python2"
   "sbcl"
   "docker-cli"
   "icedtea"
   "gcc-toolchain"
   ;; "glibc" ;; For ldd
   "curl"
   "the-silver-searcher"
   "fish"  ;; Powers Emacs fish-completion

   ;; Communication
   "pidgin"
   "telegram-purple"

   ;; Browsers
   "next"
   "vimb"

   ;; Syncthing
   "syncthing"
   "qsyncthingtray"

   ;; Mail
   "mu"
   "offlineimap"

   ;; System Tools
   "alsa-utils"
   "openssh"
   "zip"
   "unzip"
   "pinentry-emacs"

   ;; Xorg Tools
   "xev"
   "xset"
   "xrdb"
   "xhost"
   "xmodmap"
   "setxkbmap"
   "xrandr"
   "arandr"
   "xss-lock"
   "libinput"
   "xinput"

   ;; Authentication
   "password-store"
   "gnome-keyring"
   "polkit-gnome"

   ;; Work Tools
   "freerdp"

   ;; Containerized Applications
   "flatpak"  ;; For Teams, Spotify, and qutebrowser

   ;; Fonts
   "font-fira-mono"
   "font-fira-code"
   "font-abattis-cantarell"
   "font-dejavu"
   "font-gnu-freefont-ttf"
   "font-liberation"
   "font-awesome"
   "gs-fonts"

   ;; Desktop
   "feh"
   "cups"
   "scrot"
   "dmenu"
   "compton"
   "redshift"
   "gucharmap"
   "brightnessctl"
   "xdg-utils"     ;; For xdg-open, etc
   "gtk+:bin"      ;; For gtk-launch
   "glib:bin"      ;; For gio-launch-desktop
   "gtk-xfce-engine"
   "shared-mime-info"

   ;; Xfce Tools
   "xfconf"
   "xfce4-terminal"
   "xfce4-settings"
   "xfce4-notifyd"
   "xfce4-screenshooter"

   ;; GTK Themes
   "arc-theme"
   "arc-icon-theme"
   "hicolor-icon-theme"
   "gnome-icon-theme"
   "gnome-backgrounds"

   ;; Games
   "aisleriot"
   "gnome-mahjongg"

   ;; Media Players
   "mpv"
   "youtube-dl"
   "playerctl"

   ;; Video Codecs
   "gstreamer"
   "gst-plugins-base"
   "gst-plugins-good"
   "gst-plugins-bad"
   "gst-plugins-ugly"
   "gst-libav"

   ;; Audio Device Control
   "volumeicon"
   "pavucontrol"))
