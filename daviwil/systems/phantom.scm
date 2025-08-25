(define-module (daviwil systems phantom)
  #:use-module (gnu)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages file-systems)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages nvidia)
  #:use-module (nongnu services nvidia)
  #:use-module (daviwil utils)
  #:use-module (daviwil systems base)
  #:use-module (daviwil systems common)
  #:use-module (daviwil services vpn)
  #:use-module (daviwil home-services video)
  #:use-module (daviwil home-services streaming))

(operating-system
 (inherit base-operating-system)
 (host-name "phantom")

 (firmware (list linux-firmware))

 ;; (kernel-arguments
 ;;  (append
 ;;   '("modprobe.blacklist=nouveau"
 ;;     "nvidia_drm.modeset=1")
 ;;   %default-kernel-arguments))

 (mapped-devices
  (list (mapped-device
         (source (uuid "cee84d1e-3918-4148-9aac-ecd4dd1fd04f"))
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

 ;; (packages
 ;;  (cons* nvidia-exec
 ;;         %base-packages))

 (services
  (append
   (operating-system-user-services base-operating-system)
   (list
    ;; WireGuard VPN to 0x11.run server
    (wireguard-0x11-client-service "10.0.0.3/24")

    (guix-home-config
     (home-environment
       (services (cons* (service home-pipewire-service-type)
                        (service home-streaming-service-type)
                        (service home-video-service-type)
                        common-home-services))))))))

;; System-specific services
;; (service nvidia-service-type)))))
