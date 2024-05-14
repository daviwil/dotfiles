(define-module (daviwil home-services games)
  #:use-module (gnu home services)
  #:use-module (gnu packages gnome)
  #:use-module (nongnu packages game-client))

(define (home-games-profile-service config)
  (list steam
        aisleriot
        gnome-mahjongg))

(define-public home-games-service-type
  (service-type (name 'home-games)
                (description "Packages and configuration for video games.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-games-profile-service)))
                (default-value #f)))
