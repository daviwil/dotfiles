(define-module (daviwil systems zerocool)
  #:use-module (daviwil utils)
  #:use-module (daviwil systems base)
  #:use-module (daviwil systems common)
  #:use-module (daviwil home-services xsettingsd)
  #:use-module (daviwil home-services pipewire)  
  #:use-module (gnu home)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (nongnu packages linux))

(define home
  (home-environment
   (services (cons*
              (service home-xsettingsd-service-type
                       (home-xsettingsd-configuration
                        (dpi 180)))
              (service home-pipewire-service-type)
              common-home-services))))

(define system
  (operating-system
   (inherit base-operating-system)
   (host-name "zerocool")

   (mapped-devices
    (list (mapped-device
           (source (uuid "fd247c70-2dc6-48c5-872a-9bd0042a1869"))
           (target "system-root")
           (type luks-device-mapping))))

   (file-systems (cons*
                  (file-system
                   (device "/dev/mapper/system-root")
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
