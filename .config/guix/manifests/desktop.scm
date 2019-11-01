;; This file contains the list of packages that should be installed for
;; daily use after the base Guix system is online.

(specifications->manifest
 '(
   ;; The One
   "emacs"
   "emacs-pinentry"

   ;; Developer tools
   "node"
   "sbcl"
   "docker-cli"
   "gcc-toolchain"
   ;; "glibc" ;; For ldd
   "curl"
   "the-silver-searcher"

   ;; Communication
   "pidgin"
   "telegram-purple"

   ;; Browsers
   "next"
   "qutebrowser"
   "ungoogled-chromium"

   ;; Syncthing
   "syncthing"
   "qsyncthingtray"

   ;; System Tools
   "alsa-utils"
   "openssh"
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
   "gst-plugins-good"
   "gst-plugins-ugly"
   "gst-libav"

   ;; Audio Device Control
   "volumeicon"
   "pavucontrol"))
