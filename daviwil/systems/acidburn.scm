(define-module (daviwil systems acidburn)
  #:use-module (daviwil utils)
  #:use-module (daviwil home-services desktop)
  #:use-module (daviwil home-services udiskie)
  #:use-module (gnu services)
  #:use-module (gnu home)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services shells))

;; Tasks:
;; - Figure out what else I can move over from my .profile

;; TODO: Move this to shared config file
(define-public common-home-services
  (list
   ;; Set up shell environment
   (service home-bash-service-type)

   ;; Set up desktop environment
   (service home-desktop-service-type)

   ;; Udiskie for auto-mounting devices
   (service home-udiskie-service-type)

   ;; Set up my environment variables
   (simple-service 'env-vars
                   home-environment-variables-service-type
                   '( ;; Sort hidden (dot) files first in `ls` listings
                     ("LC_COLLATE" . "C")

                     ;; Emacs is our editor
                     ("VISUAL" . "emacsclient")
                     ("EDITOR" . "emacsclient")

                     ;; Add some things to $PATH (maybe integrate into other services?)
                     ("PATH" . "$HOME/.dotfiles/.bin:$HOME/.npm-global/bin:$PATH")

                     ;; Make sure Flatpak apps are visible
                     ("XDG_DATA_DIRS" . "$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share")))))

(home-environment
 (packages (gather-manifest-packages '(emacs desktop video)))
 (services common-home-services))
