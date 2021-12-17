(define-module (daviwil home-services desktop)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (daviwil packages fonts)
  #:use-module (daviwil packages linux)
  #:use-module (flat packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:export (home-desktop-service-type))

;; TODO: Add configuration for DPI setting!
(define dpi 180)

(define (home-desktop-files-service config)
  (list `("xsession" ,(local-file "xsession" #:recursive? #t))
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
  (list stumpwm+slynk
        `(,stumpwm "lib")
        stumpish
        emacs-stumpwm-mode
        sbcl-stumpwm-swm-gaps
        sbcl-stumpwm-ttf-fonts
        sbcl-stumpwm-stumptray
        sbcl-stumpwm-kbd-layouts
        sbcl

        ;; TODO: Remove when Emacs service is working
        emacs-native-comp

        ;; Sound
        pipewire-0.3.38

        ;; Appearance
        compton
        xsettingsd
        matcha-theme
        papirus-icon-theme

        ;; Fonts
        font-jost
        font-iosevka-aile
        font-jetbrains-mono))

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
    (stop #~(make-system-destructor "gpgconf --kill gpg-agent")))
   ;; Start Pipewire daemons
   (shepherd-service
    ;; (requirement '(dbus-home))
    (provision '(pipewire))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire-0.3.38 "/bin/pipewire")))))
   (shepherd-service
    (requirement '(pipewire))
    (provision '(pipewire-media-session))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire-0.3.38 "/bin/pipewire-media-session")))))
   (shepherd-service
    (requirement '(pipewire))
    (provision '(pipewire-pulse))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire-0.3.38 "/bin/pipewire-pulse")))))))

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
