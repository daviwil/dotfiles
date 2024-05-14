(define-module (daviwil systems phantom)
  #:use-module (daviwil systems base)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages file-systems)
  #:use-module (daviwil utils)
  #:use-module (daviwil systems common))

(system-config
 #:home
 (home-environment
  (packages (gather-manifest-packages '(video)))
  (services (cons* (service home-pipewire-service-type)
                   common-home-services)))

 #:system
 (operating-system
   (host-name "phantom")

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout keyboard-layout)))

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
                  %base-file-systems)))
