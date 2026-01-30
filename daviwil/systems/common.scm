(define-module (daviwil systems common)
  #:use-module (daviwil utils)
  #:use-module (daviwil home-services emacs)
  #:use-module (daviwil home-services desktop)
  #:use-module (daviwil home-services udiskie)
  #:use-module (gnu services)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services syncthing)
  #:use-module (guix gexp))

(define-public common-home-services
  (list
   ;; Set environment variables for every session
   (simple-service 'profile-env-vars-service
                   home-environment-variables-service-type
                   '( ;; Sort hidden (dot) files first in `ls` listings
                     ("LC_COLLATE" . "C")

                     ;; Emacs is our editor
                     ("VISUAL" . "emacsclient")
                     ("EDITOR" . "emacsclient")

                     ;; Add some things to $PATH (maybe integrate into other services?)
                     ("PATH" . "$HOME/.bin:$HOME/.local/bin:$HOME/.npm-global/bin:$PATH")

                     ;; Make sure Flatpak apps are visible
                     ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")

                     ;; Make sure JAVA_HOME is set
                     ;; TODO:  Move this to a different service
                     ;; ("JAVA_HOME" . "$(dirname $(dirname $(readlink $(which java))))")

                     ;; Set Wayland-specific environment variables (taken from RDE)
                     ("XDG_CURRENT_DESKTOP" . "sway")
                     ("XDG_SESSION_TYPE" . "wayland")
                     ("RTC_USE_PIPEWIRE" . "true")
                     ("SDL_VIDEODRIVER" . "wayland")
                     ("MOZ_ENABLE_WAYLAND" . "1")
                     ("CLUTTER_BACKEND" . "wayland")
                     ("ELM_ENGINE" . "wayland_egl")
                     ("ECORE_EVAS_ENGINE" . "wayland-egl")
                     ("QT_QPA_PLATFORM" . "wayland-egl")))

   ;; Set up the shell environment
   (service home-bash-service-type
            (home-bash-configuration
             (bash-profile
              `(,(plain-file "bash-profile-extras"
                             (string-append
                              ;; Load the Nix profile
                              "if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then\n"
                              "  . /run/current-system/profile/etc/profile.d/nix.sh\n"
                              "fi\n"))
                ,(plain-file "bash-sway-login"
                             (string-append
                              "if [ -z \"$WAYLAND_DISPLAY\" ] && [ \"$XDG_VTNR\" -eq 1 ]; then\n"
                              "  exec sway\n"
                              "fi\n"))))
             (bashrc
              `(,(local-file "../files/bash-prompt")))))

   ;; Place other files
   (simple-service 'profile-files-service
                   home-files-service-type
                   (list `(".inputrc" ,(local-file "../files/inputrc"))))

   ;; GnuPG configuration
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (file-append pinentry-emacs "/bin/pinentry-emacs"))
             (ssh-support? #t)
             (default-cache-ttl 28800)
             (max-cache-ttl 28800)
             (default-cache-ttl-ssh 28800)
             (max-cache-ttl-ssh 28800)))

   ;; Emacs configuration
   (service home-emacs-config-service-type)

   ;; Run user dbus session
   (service home-dbus-service-type)

   ;; Set up desktop environment
   (service home-desktop-service-type)

   ;; Start background jobs
   (service home-mcron-service-type
            (home-mcron-configuration
             (jobs
              (list
               #~(job
                  '(next-hour (range 0 24 4))
                  "~/.dotfiles/.bin/sync-passwords")
               #~(job
                  "0,30 8-17 * * 1-5"
                  "~/Tracker/chief-ping")))))

   ;; File synchronization
   (service home-syncthing-service-type)

   ;; Monitor battery levels
   (service home-batsignal-service-type)

   ;; Udiskie for auto-mounting devices
   (service home-udiskie-service-type)))
