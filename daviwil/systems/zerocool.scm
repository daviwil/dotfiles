(define-module (daviwil systems zerocool)
  #:use-module (daviwil utils)
  #:use-module (daviwil systems base)
  #:use-module (daviwil systems common)
  #:use-module (daviwil home-services audio)
  #:use-module (daviwil home-services games)
  #:use-module (daviwil home-services video)
  #:use-module (daviwil home-services finance)
  #:use-module (daviwil home-services streaming)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu services)
  #:use-module (gnu services docker)
  #:use-module (gnu system)
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (nongnu packages linux))


(system-config
 #:home
 (home-environment
  (services (cons* (service home-pipewire-service-type)
                   (service home-video-service-type)
                   ;; Removed for new due to packages that don't build
                   ;;(service home-audio-service-type)
                   (service home-finance-service-type)
                   (service home-streaming-service-type)
                   (service home-games-service-type)
                   common-home-services)))

 #:system
 (operating-system
   (host-name "zerocool")

   (firmware (list linux-firmware
                   sof-firmware
                   radeon-firmware))

   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout keyboard-layout)))

   (mapped-devices
    (list (mapped-device
           (source (uuid "cd03bf08-abcc-4037-8876-73ce1ae341cf"))
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
                  %base-file-systems))

   (services (list
              (service oci-container-service-type
                       (list
                        (oci-container-configuration
                         (image "jellyfin/jellyfin")
                         (provision "jellyfin")
                         (network "host")
                         (ports
                          '(("8096" . "8096")))
                         (volumes
                          '("jellyfin-config:/config"
                            "jellyfin-cache:/cache"
                            "/home/daviwil/Media:/media")))))))))
