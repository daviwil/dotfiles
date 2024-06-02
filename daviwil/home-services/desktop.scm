(define-module (daviwil home-services desktop)
  #:use-module (daviwil packages fonts)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages glib)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-desktop-service-type))

(define (home-desktop-profile-service config)
  (cons*
   (list glib "bin")
   (map specification->package+output
        '(;; Sway setup
          "sway"
          "swayidle"
          "swaylock"
          "waybar"
          "fuzzel"
          "mako"
          "gammastep"
          "grimshot" ;; grimshot --notify copy area
          "network-manager-applet"

          ;; Compatibility for older Xorg applications
          "xorg-server-xwayland"

          ;; Flatpak and XDG utilities
          "flatpak"
          "xdg-desktop-portal"
          "xdg-desktop-portal-gtk"
          "xdg-desktop-portal-wlr"
          "xdg-utils" ;; For xdg-open, etc
          "xdg-dbus-proxy"
          "shared-mime-info"

          ;; Appearance
          "matcha-theme"
          "papirus-icon-theme"
          "breeze-icons" ;; For KDE apps
          "gnome-themes-extra"
          "adwaita-icon-theme"

          ;; Fonts
          "font-jost"
          "font-iosevka-ss08"
          "font-iosevka-aile"
          "font-jetbrains-mono"
          "font-google-noto"
          "font-google-noto-emoji"
          "font-liberation"
          "font-awesome"

          ;; Browsers
          "qtwayland@5"
          "qutebrowser"
          "vimb"

          ;; Authentication
          "password-store"

          ;; Audio devices and media playback
          "mpv"
          "mpv-mpris"
          "youtube-dl"
          "playerctl"
          "gstreamer"
          "gst-plugins-base"
          "gst-plugins-good"
          "gst-plugins-bad"
          "gst-plugins-ugly"
          "gst-libav"
          "alsa-utils"
          "pavucontrol"

          ;; Graphics
          "gimp"

          ;; PDF reader
          "zathura"
          "zathura-pdf-mupdf"

          ;; File syncing
          "syncthing-gtk"

          ;; General utilities
          "curl"
          "wget"
          "openssh"
          "zip"
          "unzip"
          "trash-cli"))))

(define (home-desktop-environment-variables config)
  '(("_JAVA_AWT_WM_NONREPARENTING" . "1")))

(define home-desktop-service-type
  (service-type (name 'home-desktop)
                (description "My desktop environment service.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-desktop-profile-service)
                       (service-extension
                        home-environment-variables-service-type
                        home-desktop-environment-variables)))
                (default-value #f)))
