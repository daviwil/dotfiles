(define-module (daviwil home-services desktop)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages glib)
  #:use-module (daviwil packages fonts)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)

  #:export (home-desktop-service-type))

(define (home-desktop-profile-service config)
  (cons*
   ;; NOTE: This is a very weird hack to get around an issue where "glib:bin"
   ;; returns a newer version of glib than what most packages are using via the
   ;; exported `glib' symbol.  The "bin" output is needed to get the `gsettings'
   ;; program to control GTK theme settings without a configuration file.
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
          "feh"
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

          ;; TODO: Remove when Emacs service is working
          "emacs-next-pgtk"

          ;; Appearance
          "matcha-theme"
          "papirus-icon-theme"
          "breeze-icons" ;; For KDE apps

          ;; Fonts
          "font-jost"
          "font-iosevka-ss08"
          "font-iosevka-aile"
          "font-jetbrains-mono"
          "font-google-noto"
          "font-google-noto-emoji"
          "font-liberation"
          "font-awesome"
          "gucharmap"
          "fontmanager"

          ;; Browsers
          "qtwayland@5"
          "qutebrowser"

          ;; Authentication
          "password-store"

          ;; Audio devices and media playback
          "mpv"
          ;; "mpv-mpris"
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
          "syncthing"
          "syncthing-gtk"

          ;; General utilities
          "curl"
          "wget"
          "openssh"
          "zip"
          "unzip"
          "trash-cli"))))

(define (home-desktop-shepherd-services config)
  (list
   ;; TODO: Use built-in syncthing service
   (shepherd-service
    (provision '(syncthing))
    (documentation "Run and control syncthing.")
    (start #~(make-forkexec-constructor '("syncthing" "-no-browser")))
    (stop #~(make-kill-destructor)))))

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
                        home-shepherd-service-type
                        home-desktop-shepherd-services)
                       (service-extension
                        home-environment-variables-service-type
                        home-desktop-environment-variables)))
                (default-value #f)))
