(define-module (daviwil home-services desktop)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (daviwil packages fonts)
  #:use-module (flat packages emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:export (home-desktop-service-type))

;; TODO: Add configuration for DPI setting!
(define dpi 180)

(define (home-desktop-files-service config)
  (list `(".xsession" ,(local-file "xsession" #:recursive? #t))
        `(".config/xsettingsd/xsettingsd.conf" ,(plain-file "xsettingsd.conf" (string-append "
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

        herbstluftwm
        rofi
        polybar

        ;; TODO: Remove when Emacs service is working
        emacs-native-comp

        ;; Sound
        pipewire-0.3
        wireplumber

        ;; Appearance
        compton
        xsettingsd
        matcha-theme
        papirus-icon-theme

        ;; Fonts
        font-jost
        font-iosevka-aile
        font-jetbrains-mono))


(define (home-desktop-xdg-configuration-services config)
  `(("alsa/asoundrc"
     ,(mixed-text-file
       "asoundrc"
       #~(string-append
          "<"
	        #$(file-append
             pipewire-0.3 "/share/alsa/alsa.conf.d/50-pipewire.conf")
	        ">\n<"
	        #$(file-append
             pipewire-0.3 "/share/alsa/alsa.conf.d/99-pipewire-default.conf")
          ">\n"
          "
pcm_type.pipewire {
  lib " #$(file-append pipewire-0.3 "/lib/alsa-lib/libasound_module_pcm_pipewire.so")
  "
}
ctl_type.pipewire {
  lib " #$(file-append pipewire-0.3 "/lib/alsa-lib/libasound_module_ctl_pipewire.so")
  "
}
")))))

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
   ;; Start Pipewire daemon
   (shepherd-service
    (provision '(pipewire))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire-0.3 "/bin/pipewire"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/pipewire.log")
              #:environment-variables
              (append (list "DISABLE_RTKIT=1")
                      (default-environment-variables)))))
   ;; Start Pipewire PulseAudio module
   (shepherd-service
    (requirement '(pipewire))
    (provision '(pipewire-pulse))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append pipewire-0.3 "/bin/pipewire-pulse"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/pipewire-pulse.log")
              #:environment-variables
              (append (list "DISABLE_RTKIT=1")
                      (default-environment-variables)))))
   ;; Start Wireplumber session manager
   (shepherd-service
    (requirement '(pipewire))
    (provision '(wireplumber))
    (stop  #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append wireplumber "/bin/wireplumber"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/wireplumber.log")
             #:environment-variables
             (append (list "DISABLE_RTKIT=1")
                     (default-environment-variables)))))))

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
                        home-xdg-configuration-files-service-type
                        home-desktop-xdg-configuration-services)
                       (service-extension
                        home-environment-variables-service-type
                        home-desktop-environment-variables)))
                (default-value #f)))

   ;; #:use-module (gnu home services state)
   ;; Set up the Chemacs repo as ~/.config/emacs
   ;; NOTE: Not working at the moment
   ;; (service home-state-service-type
   ;;          (list
   ;;           (state-git "/home/daviwil/.config/emacs"
   ;;                      "https://github.com/plexus/chemacs2")))
