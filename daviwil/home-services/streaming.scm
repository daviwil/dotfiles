(define-module (daviwil home-services streaming)
  #:use-module (nongnu packages video)
  #:use-module (daviwil packages video)
  #:use-module (gnu home services))

(define (home-streaming-profile-service config)
  (list obs-with-cef
        obs-vaapi))

(define-public home-streaming-service-type
  (service-type (name 'home-streaming)
                (description "Packages and configuration for streaming with OBS.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-streaming-profile-service)))
                (default-value #f)))
