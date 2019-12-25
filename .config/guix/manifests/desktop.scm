;; This file contains the list of packages for use in my day-to-day desktop
;; environment.

(specifications->manifest
 '(;; Browsers
   "vimb"

   ;; Syncthing
   "syncthing"
   "qsyncthingtray"

   ;; Mail
   "mu"
   "offlineimap"

   ;; System Tools
   "openssh"
   "zip"
   "unzip"

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

   ;; Fonts
   "font-fira-mono"
   "font-fira-code"
   "font-abattis-cantarell"
   "font-dejavu"
   "font-gnu-freefont-ttf"
   "font-liberation"
   "font-awesome"
   "gs-fonts"

   ;; Document Readers
   "evince"

   ;; Images and Screenshots
   "feh"
   "scrot"

   ;; Printing
   "cups"

   ;; Desktop
   "compton"
   "redshift"
   "gucharmap"
   "brightnessctl"
   "xdg-utils"     ;; For xdg-open, etc
   "gtk+:bin"      ;; For gtk-launch
   "glib:bin"      ;; For gio-launch-desktop
   "shared-mime-info"

   ;; Xfce Tools
   "xfconf"
   "xfce4-terminal"
   "xfce4-settings"
   "xfce4-notifyd"
   "xfce4-screenshooter"
   "gtk-xfce-engine"

   ;; GTK Themes
   "arc-theme"
   "arc-icon-theme"
   "matcha-theme"
   "hicolor-icon-theme"
   "gnome-icon-theme"
   "gnome-backgrounds"
   "papirus-icon-theme"

   ;; Authentication
   "password-store"

   ;; Containerized Applications
   "flatpak"  ;; For Teams and Spotify

   ;; Media Players
   "mpv"
   "mpv-mpris"
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
   "alsa-utils"
   "pavucontrol"))
