;; This file contains the list of packages that should be installed for
;; daily use after the base Guix system is online.

(specifications->manifest
 '(
   ;; The One
   "emacs"

   ;; Developer tools
   "glibc" ;; For ldd
   "curl"

   ;; Communication
   "pidgin"
   "telegram-purple"

   ;; Browsers
   "next"
   "qutebrowser"

   ;; Syncthing
   "syncthing"
   "qsyncthingtray"

   ;; System Tools
   "alsa-utils"
   "openssh"

   ;; Xorg Tools
   "xhost"
   "xset"
   "setxkbmap"
   "xrandr"
   "arandr"

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

   ;; Video Players
   "mpv"
   "youtube-dl"

   ;; Video Codecs
   "gstreamer"
   "gst-plugins-good"
   "gst-plugins-ugly"
   "gst-libav"

   ;; Audio Device Control
   "volumeicon"
   "pavucontrol"))
