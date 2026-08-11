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
                     ("PATH" . "$HOME/.sigil/bin:$HOME/.bin:$HOME/.local/bin:$HOME/.npm-global/bin:$PATH")

                     ;; Route GPG pinentry to the current terminal
                     ("GPG_TTY" . "$(tty)")

                     ;; sudo askpass via tmux popup (use sudo -A)
                     ("SUDO_ASKPASS" . "$HOME/.local/bin/sudo-askpass-tmux")

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
                ;; Auto-start Sway, but ONLY from a real virtual terminal.
                ;;
                ;; The guard keys on the CONTROLLING TERMINAL, not on XDG_VTNR,
                ;; because XDG_VTNR is inherited by every child of the session
                ;; -- it is set even in processes with no tty at all.  Any login
                ;; shell spawned under a daemon that was itself started from the
                ;; VT1 session therefore saw XDG_VTNR=1, and with WAYLAND_DISPLAY
                ;; absent from that daemon's environment both conditions passed
                ;; and the shell exec'd sway.  Caught 2026-08-11 opening a remote
                ;; terminal from a phone through the Slate node: sway started,
                ;; failed to take the seat ("Could not take control of session:
                ;; Device or resource busy"), and the session died at once.
                ;;
                ;; A pseudo-terminal is /dev/pts/N and never /dev/tty1, so this
                ;; form cannot be reached by an inherited variable.
                ,(plain-file "bash-sway-login"
                             (string-append
                              "if [ -z \"$WAYLAND_DISPLAY\" ] && [ \"$(tty)\" = \"/dev/tty1\" ]; then\n"
                              "  exec sway\n"
                              "fi\n"))))
             (bashrc
              `(,(local-file "../files/bash-prompt")
                ,(plain-file "gpg-tty-update"
                             (string-append
                              ;; Tell gpg-agent the current tty for pinentry routing\n"
                              "gpg-connect-agent updatestartuptty /bye >/dev/null 2>&1\n"))))))

   ;; Place other files
   (simple-service 'profile-files-service
                   home-files-service-type
                   (list `(".inputrc" ,(local-file "../files/inputrc"))
                         `(".local/bin/sudo-askpass-tmux"
                           ,(computed-file "sudo-askpass-tmux"
                              #~(begin
                                  (copy-file #$(local-file "../files/sudo-askpass-tmux")
                                             #$output)
                                  (chmod #$output #o755))))))

   ;; GnuPG configuration — use pinentry-tmux for passphrase prompts
   ;; in tmux sessions, with pinentry-curses as a fallback
   (service home-gpg-agent-service-type
            (home-gpg-agent-configuration
             (pinentry-program
              (computed-file "pinentry-tmux"
                #~(begin
                    (copy-file #$(local-file "../files/pinentry-tmux")
                               #$output)
                    (chmod #$output #o755))))
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
                  "~/.dotfiles/.bin/sync-passwords")))))

   ;; File synchronization
   (service home-syncthing-service-type)

   ;; Monitor battery levels
   (service home-batsignal-service-type)

   ;; Udiskie for auto-mounting devices
   (service home-udiskie-service-type)))
