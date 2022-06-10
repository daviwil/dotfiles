(define-module (daviwil home-services udiskie)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)

  #:export (home-udiskie-service-type))

(define (home-udiskie-profile-service config)
  (list udiskie))

(define (home-udiskie-shepherd-service config)
  (list
   (shepherd-service
    (provision '(udiskie))
    (documentation "Run and control udiskie.")
    (start #~(make-forkexec-constructor '("udiskie" "-t")))
    (stop #~(make-kill-destructor)))))

(define home-udiskie-service-type
  (service-type (name 'home-udiskie)
                (description "A service for launching Udiskie.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-udiskie-profile-service)
                       (service-extension
                        home-shepherd-service-type
                        home-udiskie-shepherd-service)))
                (default-value #f)))
