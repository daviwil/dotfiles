;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

(define-module (phantom)
  #:use-module (base-system)
  #:use-module (gnu))

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
                %base-file-systems)))
