(define-module (daviwil home-services video)
  #:use-module (gnu packages video)
  #:use-module (gnu home services))

(define (home-video-profile-service config)
  (list ffmpeg
        ;; Get details about webcams: v4l2-ctl --list-devices
        v4l-utils))

(define-public home-video-service-type
  (service-type (name 'home-video)
                (description "Packages and configuration for video editing.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-video-profile-service)))
                (default-value #f)))
