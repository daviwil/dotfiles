(define-module (daviwil home-services desktop)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (daviwil packages fonts)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:export (home-desktop-service-type))

(define (home-desktop-files-service config)
  (list `(".xsession" ,(local-file "xsession" #:recursive? #t))))

(define currently-unused
  '("stumpwm+slynk"
    "stumpwm:lib"
    "stumpish"
    "emacs-stumpwm-mode"
    "sbcl-stumpwm-swm-gaps"
    "sbcl-stumpwm-ttf-fonts"
    "sbcl-stumpwm-stumptray"
    "sbcl-stumpwm-kbd-layouts"
    "sbcl"))

(define (home-desktop-profile-service config)
  (map specification->package
       '(;; New herbstluft setup
         "herbstluftwm"
         "rofi"
         "polybar"
         "dunst"

         ;; Sway setup
         "sway"
         "swayidle"
         "waybar"
         "fuzzel"
         "gammastep"
         "xdg-desktop-portal"
         "xdg-desktop-portal-gtk"
         "xdg-desktop-portal-wlr"
         "glib:bin"                     ; For gsettings

         ;; General tools
	       "feh"
         "arandr"
         "autorandr"
         "xset"
         "xhost"
         "xss-lock"
	       "dbus"

         ;; Controlling audio players
         "playerctl"

         ;; TODO: Remove when Emacs service is working
         "emacs-next-pgtk"
         "emacs-exwm"

         ;; Appearance
         "compton"
         "xsettingsd"
         "matcha-theme"
         "papirus-icon-theme"

         ;; Fonts
         "font-jost"
         "font-iosevka-aile"
         "font-jetbrains-mono")))

(define (home-desktop-shepherd-services config)
  (list
   ;; TODO: Use built-in syncthing service
   (shepherd-service
    (provision '(syncthing))
    (documentation "Run and control syncthing.")
    (start #~(make-forkexec-constructor '("syncthing" "-no-browser")))
    (stop #~(make-kill-destructor)))
   ;; TODO: Make this a separate service or reuse from RDE
   (shepherd-service
    (provision '(gpg-agent))
    (documentation "Run and control gpg-agent.")
    (start #~(make-system-constructor "gpg-connect-agent /bye"))
    (stop #~(make-system-destructor "gpgconf --kill gpg-agent")))))

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
                        home-files-service-type
                        home-desktop-files-service)
                       (service-extension
                        home-environment-variables-service-type
                        home-desktop-environment-variables)))
                (default-value #f)))
