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
   "xhost"
   "xset"
   "xrdb"
   "setxkbmap"
   "xrandr"
   "arandr"
   "xss-lock"

   ;; Authentication
   "password-store"
   "gnome-keyring"
   "polkit-gnome"

   ;; Containerized Applications
   "flatpak"

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
   "dmenu"
   "gnome-backgrounds"
   "compton"
   "nitrogen"
   "redshift"
   "brightnessctl"
   "xfce4-notifyd"
   "xdg-utils"     ;; For xdg-open, etc
   "gtk+:bin"      ;; For gtk-launch
   "glib:bin"      ;; For gio-launch-desktop

   ;; GTK Themes
   "arc-theme"
   "arc-icon-theme"

   ;; Screenshots
   "xfce4-screenshooter"
   "scrot"

   ;; Games
   "aisleriot" ;; The binary is called 'sol'!
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
