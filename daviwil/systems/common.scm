(define-module (daviwil systems common)
  #:use-module (daviwil utils)
  #:use-module (daviwil home-services desktop)
  #:use-module (daviwil home-services udiskie)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services shells)
  #:use-module (guix gexp))

(define-public common-home-services
  (list
   ;; Set up the shell environment
   (service home-bash-service-type
            (home-bash-configuration
             (bash-profile
              `(,(plain-file "bash-profile-extras"
                             (string-append
                              ;; Load the Nix profile
                              "if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then"
                              "  . /run/current-system/profile/etc/profile.d/nix.sh"
                              "fi"

                              ;; Don't use the system-level PulseAudio configuration
                              "unset PULSE_CONFIG"
                              "unset PULSE_CLIENTCONFIG"))))

             (environment-variables
              '( ;; Sort hidden (dot) files first in `ls` listings
                ("LC_COLLATE" . "C")

                ;; Emacs is our editor
                ("VISUAL" . "emacsclient")
                ("EDITOR" . "emacsclient")

                ;; Add some things to $PATH (maybe integrate into other services?)
                ("PATH" . "$HOME/.dotfiles/.bin:$HOME/.npm-global/bin:$PATH")

                ;; Make sure Flatpak apps are visible
                ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")

                ;; Make sure JAVA_HOME is set
                ;; TODO:  Move this to a different service
                ("JAVA_HOME" . "$(dirname $(dirname $(readlink $(which java))))")

                ;; Set the SSH authentication socket
                ;; TODO: Move to a gpg service
                ("SSH_AUTH_SOCK" . "$(gpgconf --list-dirs agent-ssh-socket)")))))

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

   ;; Udiskie for auto-mounting devices
   (service home-udiskie-service-type)))
