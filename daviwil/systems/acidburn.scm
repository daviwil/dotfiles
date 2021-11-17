(define-module (daviwil systems acidburn)
  #:use-module (daviwil utils)
  #:use-module (daviwil home-services desktop)
  #:use-module (daviwil home-services udiskie)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)

;; TODO: Move this to shared config file
(define-public common-home-services
  (list
   ;; Set up the shell environment
   (service home-bash-service-type
            (home-bash-configuration
             (bash-profile
              '( ;; Load the Nix profile
                "if [ -f /run/current-system/profile/etc/profile.d/nix.sh ]; then"
                "  . /run/current-system/profile/etc/profile.d/nix.sh"
                "fi"

                ;; Don't use the system-level PulseAudio configuration
                "unset PULSE_CONFIG"
                "unset PULSE_CLIENTCONFIG"))

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

   ;; Udiskie for auto-mounting devices
   (service home-udiskie-service-type)))

(home-environment
 (packages (gather-manifest-packages '(emacs desktop video)))
 (services common-home-services))
