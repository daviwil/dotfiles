(define-module (daviwil systems acidburn)
  #:use-module (daviwil utils)
  #:use-module (daviwil systems base)
  #:use-module (daviwil systems common)
  #:use-module (daviwil home-services xsettingsd)
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

(define home
  (home-environment
   (packages (gather-manifest-packages '(mail video games)))
   (services (cons* (service home-pipewire-service-type)
                    common-home-services))))

(define system
  (operating-system
   (inherit base-operating-system)
   (host-name "acidburn")

   ;; Add sof-firmware drivers for audio on ThinkPad X1 Nano
   (firmware (list linux-firmware sof-firmware))

   (mapped-devices
    (list (mapped-device
           (source (uuid "e3c10e06-9667-46be-82b8-4ca68873b416"))
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

   (services (cons*
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
                            "/home/daviwil/Media:/media")))))

              (operating-system-user-services base-operating-system)))))

;; Return home or system config based on environment variable
(if (getenv "RUNNING_GUIX_HOME") home system)
