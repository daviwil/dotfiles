(define-module (daviwil systems acidburn)
  #:use-module (daviwil utils)
  #:use-module (daviwil systems base)
  #:use-module (daviwil systems common)
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
   (packages (gather-manifest-packages '(emacs desktop music video games)))
   (services (append
              common-home-services
              (list (service home-xsettingsd-service-type
                             (home-xsettingsd-configuration
                              (dpi 180)))
                    (service home-pipewire-service-type))))))

(define system
  (operating-system
   (inherit base-operating-system)
   (host-name "acidburn")

   ;; Add sof-firmware drivers for audio on ThinkPad X1 Nano
   (firmware (list linux-firmware sof-firmware))

   (mapped-devices
    (list (mapped-device
           (source (uuid "15ece913-c423-49aa-ac42-3bad39fdd966"))
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
