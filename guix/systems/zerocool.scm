(use-modules (gnu)
             (gnu system nss)
             (gnu services pm)
             (gnu services desktop)
             (nongnu packages linux))

(use-service-modules desktop xorg)
(use-package-modules certs)

(operating-system
  (host-name "zerocool")
  (timezone "America/Los_Angeles")
  (locale "en_US.utf8")

  ;; Use non-free Linux and firmware
  (kernel linux)
  (firmware (list linux-firmware))

  ;; Choose US English keyboard layout.  The "altgr-intl"
  ;; variant provides dead keys for accented characters.
  (keyboard-layout (keyboard-layout "us" "altgr-intl"))

  ;; Use the UEFI variant of GRUB with the EFI System
  ;; Partition mounted on /boot/efi.
  (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (target "/boot/efi")
                (keyboard-layout keyboard-layout)))

  ;; Specify a mapped device for the encrypted root partition.
  ;; The UUID is that returned by 'cryptsetup luksUUID'.
  (mapped-devices
   (list (mapped-device
          (source (uuid "12345678-1234-1234-1234-123456789abc"))
          (target "root-fs")
          (type luks-device-mapping))))

  (file-systems (cons*
                 (file-system
                    (device (file-system-label "root-fs"))
                    (mount-point "/")
                    (type "ext4")
                    (dependencies mapped-devices))
                 (file-system
                    (device (uuid "9421-68BB" 'fat))
                    (mount-point "/boot/efi")
                    (type "vfat"))
                 %base-file-systems))

  (users (cons (user-account
                (name "daviwil")
                (comment "David Wilson")
                (group "users")
                (supplementary-groups '("wheel" "netdev" "kvm"
                                        "audio" "video")))
               %base-user-accounts))

  ;; Install system-wide packages
  (packages (append (list
                     stow
                     qutebrowser
                     i3-wm
                     i3lock
                     emacs
                     pidgin
                     telegram-purple
                     syncthing
                     redshift
                     pavucontrol
                     cbatticon
                     volumeicon
                     ;; for HTTPS access
                     nss-certs
                     ;; for user mounts
                     gvfs)
                    %base-packages))

  ;; Use the "desktop" services, which include the X11 log-in service,
  ;; networking with NetworkManager, and more
  (services (cons* (service xfce-desktop-service-type
                      (set-xorg-configuration
                        (xorg-configuration
                         (keyboard-layout keyboard-layout))))
                   (service tlp-service-type)
                   (service thermald-service-type)
                   (bluetooth-service #:auto-enable t)
                   %desktop-services))

  ;; Allow resolution of '.local' host names with mDNS
  (name-service-switch %mdns-host-lookup-nss))
