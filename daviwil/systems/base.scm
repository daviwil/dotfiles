(define-module (daviwil systems base)
  #:use-module (gnu)
  #:use-module (srfi srfi-1)
  #:use-module (gnu system nss)
  #:use-module (gnu services pm)
  #:use-module (gnu services cups)
  #:use-module (gnu services guix)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages package-management)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu system linux-initrd))

(use-service-modules nix)
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

;; Modify configurations of default %desktop-services
(define %my-desktop-services
  (modify-services %desktop-services
                   ;; Configure the substitute server for the Nonguix repo
                   (guix-service-type
                    config =>
                    (guix-configuration
                     (inherit config)
                     (substitute-urls
                      (append (list "https://substitutes.nonguix.org")
                              %default-substitute-urls))
                     (authorized-keys
                      (append (list (plain-file "nonguix.pub" "(public-key
 (ecc
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
  )
 )"))
                              %default-authorized-guix-keys))))

                   ;; Suspend the machine when the laptop lid is closed
                   (elogind-service-type config =>
                                         (elogind-configuration (inherit config)
                                                                (handle-lid-switch-external-power 'suspend)))

                   ;; Enable backlight control rules for users
                   (udev-service-type config =>
                                      (udev-configuration (inherit config)
                                                          (rules (cons %backlight-udev-rule
                                                                       (udev-configuration-rules config)))))

                   ;; Add OpenVPN support to NetworkManager
                   (network-manager-service-type config =>
                                                 (network-manager-configuration (inherit config)
                                                                                (vpn-plugins (list network-manager-openvpn))))))

;; Define a libinput configuration that works well for modern touchpads
(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define-public base-operating-system
  (operating-system
   (host-name "hackstock")
   (timezone "Europe/Athens")
                                        ;(timezone "America/Los_Angeles")
   (locale "en_US.utf8")

   ;; Use non-free Linux and firmware
   (kernel linux)
   (firmware (list linux-firmware))
   (initrd microcode-initrd)

   ;; Additional kernel modules
   (kernel-loadable-modules (list v4l2loopback-linux-module))

   ;; Choose US English keyboard layout.  The "altgr-intl"
   ;; variant provides dead keys for accented characters.
   (keyboard-layout (keyboard-layout "us" "altgr-intl" #:model "thinkpad"))

   ;; Use the UEFI variant of GRUB with the EFI System
   ;; Partition mounted on /boot/efi.
   (bootloader (bootloader-configuration
                (bootloader grub-efi-bootloader)
                (targets '("/boot/efi"))
                (keyboard-layout keyboard-layout)))


   ;; Guix doesn't like it when there isn't a file-systems
   ;; entry, so add one that is meant to be overridden
   (file-systems (cons*
                  (file-system
                   (mount-point "/tmp")
                   (device "none")
                   (type "tmpfs")
                   (check? #f))
                  %base-file-systems))

   (users (cons (user-account
                 (name "daviwil")
                 (comment "David Wilson")
                 (group "users")
                 (home-directory "/home/daviwil")
                 (supplementary-groups '("wheel" ;; sudo
                                         "netdev" ;; network devices
                                         "kvm"
                                         "tty"
                                         "input"
                                         "docker"
                                         "realtime" ;; Enable realtime scheduling
                                         "lp"       ;; control bluetooth devices
                                         "audio"    ;; control audio devices
                                         "video"))) ;; control video devices

                %base-user-accounts))

   ;; Add the 'realtime' group
   (groups (cons (user-group (system? #t) (name "realtime"))
                 %base-groups))

   ;; Install bare-minimum system packages
   (packages (append (map specification->package
                          '("git"
                            "ntfs-3g"
                            "exfat-utils"
                            "fuse-exfat"
                            "stow"
                            "vim"
                            "emacs"
                            "xterm"
                            "bluez"
                            "bluez-alsa"
                            "tlp"
                            "xf86-input-libinput"
                            "nss-certs" ;; SSL root certificates
                            "gvfs"))     ;; Enable user mounts
                     %base-packages))

   ;; Use the "desktop" services, which include the X11 log-in service,
   ;; networking with NetworkManager, and more
   (services (cons* (service slim-service-type
                             (slim-configuration
                              (xorg-configuration
                               (xorg-configuration
                                (keyboard-layout keyboard-layout)
                                (extra-config (list %xorg-libinput-config))))))

                    ;; Power and thermal management services
                    (service thermald-service-type)
                    (service tlp-service-type
                             (tlp-configuration
                              (cpu-boost-on-ac? #t)
                              (wifi-pwr-on-bat? #t)))

                    ;; Enable JACK to enter realtime mode
                    (pam-limits-service
                     (list
                      (pam-limits-entry "@realtime" 'both 'rtprio 99)
                      (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)))

                    ;; Enable /usr/bin/env in shell scripts
                    (extra-special-file "/usr/bin/env"
                                        (file-append coreutils "/bin/env"))

                    ;; Enable Docker containers and virtual machines
                    (service docker-service-type)
                    (service libvirt-service-type
                             (libvirt-configuration
                              (unix-sock-group "libvirt")
                              (tls-port "16555")))

                    ;; Enable the printing service
                    (service cups-service-type
                             (cups-configuration
                              (web-interface? #t)
                              (extensions
                               (list cups-filters))))

                    ;; Add udev rules to enable PipeWire use
                    (udev-rules-service 'pipewire-add-udev-rules pipewire-0.3)

                    ;; Enable the build service for Nix package manager
                    (service nix-service-type)

                    ;; Enable the bluetooth service
                    (bluetooth-service #:auto-enable? #t)

                    ;; Remove GDM, we're using SLiM instead
                    (remove (lambda (service)
                              (eq? (service-kind service) gdm-service-type))
                            %my-desktop-services)))

   ;; Allow resolution of '.local' host names with mDNS
   (name-service-switch %mdns-host-lookup-nss)))
