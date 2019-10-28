(use-modules (gnu)
             (srfi srfi-1)
             (gnu system nss)
             (gnu services pm)
             (gnu services desktop)
             ;; (gnu services docker)
             (gnu packages wm)
             (gnu packages vim)
             (gnu packages gtk)
             (gnu packages gnome)
             (gnu packages mtools)
             (gnu packages linux)
             (gnu packages audio)
             (gnu packages gnuzilla)
             (gnu packages pulseaudio)
             (gnu packages web-browsers)
             (gnu packages version-control)
             (gnu packages package-management)
             (nongnu packages linux))

(use-service-modules desktop xorg)
(use-package-modules certs)
(use-package-modules shells)

;; Allow members of the "video" group to change the screen brightness.
(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
                  "\n"
                  "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
                  "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %my-desktop-services
  (modify-services %desktop-services
                   (udev-service-type config =>
                                      (udev-configuration (inherit config)
                                                          (rules (cons %backlight-udev-rule
                                                                       (udev-configuration-rules config)))))))

(operating-system
 (host-name "davinci")
 (timezone "America/Los_Angeles")
 (locale "en_US.utf8")

 ;; Use non-free Linux and firmware
 (kernel linux)
 (firmware (list linux-firmware))

 ;; Choose US English keyboard layout.  The "altgr-intl"
 ;; variant provides dead keys for accented characters.
 (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))

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
         (source (uuid "eaba53d9-d7e5-4129-82c8-df28bfe6527e"))
         (target "system-root")
         (type luks-device-mapping))))

 (file-systems (cons*
                (file-system
                 (device (file-system-label "system-root"))
                 (mount-point "/")
                 (type "ext4")
                 (dependencies mapped-devices))
                (file-system
                 (device "/dev/nvme0n1p2")
                 (mount-point "/boot/efi")
                 (type "vfat"))
                %base-file-systems))

 (users (cons (user-account
               (name "daviwil")
               (comment "David Wilson")
               (group "users")
               (home-directory "/home/daviwil")
               ;(shell #~(string-append #$zsh "/bin/zsh"))
               (supplementary-groups '(
                                       "wheel"     ;; sudo
                                       "netdev"    ;; network devices
                                       "kvm"
                                       "tty"
                                       "input"
                                       ;; "docker"
                                       "lp"        ;; control bluetooth devices
                                       "audio"     ;; control audio devices
                                       "video")))  ;; control video devices

              %base-user-accounts))

 ;; Install bare-minimum system packages
 (packages (append (list
                    git
                    zsh
                    exfat-utils
                    fuse-exfat
                    icecat
                    stow
                    vim
                    i3-wm
                    i3status
                    bluez
                    bluez-alsa
                    pulseaudio
                    tlp
                    nss-certs     ;; for HTTPS access
                    gvfs)         ;; for user mounts
                   %base-packages))

 ;; Use the "desktop" services, which include the X11 log-in service,
 ;; networking with NetworkManager, and more
 (services (cons* (service xfce-desktop-service-type)
                  (service slim-service-type
                           (slim-configuration
                              (xorg-configuration
                                (xorg-configuration
                                   (keyboard-layout keyboard-layout)))))
                  (service tlp-service-type
                           (tlp-configuration
                              (cpu-boost-on-ac? #t)
                              (wifi-pwr-on-bat? #t)))
                  (service thermald-service-type)
                  ;; (service docker-service-type)
                  (bluetooth-service #:auto-enable? #t)
                  (remove (lambda (service)
                             (eq? (service-kind service) gdm-service-type))
                          %my-desktop-services)))

 ;; Allow resolution of '.local' host names with mDNS
 (name-service-switch %mdns-host-lookup-nss))
