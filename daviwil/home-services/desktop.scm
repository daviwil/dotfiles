(define-module (daviwil home-services desktop)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (daviwil packages fonts)
  #:use-module (gnu services)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shepherd)
  #:use-module (guix gexp)
  #:export (home-desktop-service-type))

;; TODO: Add configuration for DPI setting!
(define dpi 180)

(define (home-desktop-files-service config)
  (list `("xsession" ,(local-file "xsession" #:recursive? #t))
        `("config/pulse/default.pa" ,(local-file "../../.config/pulse/default.pa"))
        `("config/pulse/client.conf" ,(local-file "../../.config/pulse/client.conf"))
        `("config/pulse/daemon.conf" ,(local-file "../../.config/pulse/daemon.conf"))
        `("config/xsettingsd/xsettingsd.conf" ,(plain-file "xsettingsd.conf" (string-append "
Net/ThemeName \"Matcha-dark-azul\"
Net/IconThemeName \"Papirus-Dark\"
Gtk/DecorationLayout \"menu:minimize,maximize,close\"
Gtk/FontName \"Iosevka Aile 11\"
Gtk/MonospaceFontName \"JetBrains Mono 10\"
Gtk/CursorThemeName \"Adwaita\"
Xft/Antialias 1
Xft/Hinting 0
Xft/HintStyle \"hintnone\"
Xft/DPI " (number->string (* 1024 dpi)) " # 1024 * DPI")))))

(define (home-desktop-profile-service config)
  (list emacs-exwm
        compton
        xsettingsd
        matcha-theme
        font-jost
        font-iosevka-aile
        font-jetbrains-mono
        papirus-icon-theme))

(define (home-desktop-shepherd-services config)
  (list
   ;; TODO: Confiure system service instead
   (shepherd-service
    (provision '(pulseaudio))
    (documentation "Run and control pulseaudio.")
    (start #~(make-forkexec-constructor '("pulseaudio")))
    (stop #~(make-kill-destructor)))
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
