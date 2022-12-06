(define-module (daviwil systems phantom)
  #:use-module (daviwil systems base)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages file-systems)
  #:use-module (daviwil utils)
  #:use-module (daviwil home-services pipewire)
  #:use-module (daviwil home-services xsettingsd)
  #:use-module (daviwil home-services emacs base)
  #:use-module (daviwil systems common))

(define home
  (home-environment
   (packages (gather-manifest-packages '(emacs desktop video music)))
   (services (append
              common-home-services
              (list (service home-xsettingsd-service-type
                             (home-xsettingsd-configuration
                              (dpi 140)))
                    (service home-pipewire-service-type))))))

(define system
  (operating-system
   (inherit base-operating-system)
   (host-name "phantom")

   (mapped-devices
    (list (mapped-device
           (source (uuid "091b8ad5-efb3-4c5b-8370-7db99c404a30"))
           (target "system-root")
           (type luks-device-mapping))))

   (file-systems (cons*
                  (file-system
                   (device (file-system-label "system-root"))
                   (mount-point "/")
                   (type "ext4")
                   (dependencies mapped-devices))
                  (file-system
                   (device "/dev/nvme0n1p1")
                   (mount-point "/boot/efi")
                   (type "vfat"))
                  %base-file-systems))))

;; Return home or system config based on environment variable
(if (getenv "RUNNING_GUIX_HOME") home system)
