(define-module (daviwil packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages vulkan)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp))

;; A temporary fix based on this patch:
;; https://lists.gnu.org/archive/html/guix-patches/2021-10/msg00762.html

(define-public pipewire-0.3.38
  (package
    (inherit pipewire)
    (name "pipewire")
    (version "0.3.38")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PipeWire/pipewire")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15alnzwdgjbjrmphyinwdbaa1bi9cqbk8gkhrb4y6akjqph76hs0"))))
    (arguments
     `(#:meson ,meson-0.59
       #:configure-flags
       (list (string-append "-Dudevrulesdir=" (assoc-ref %outputs "out")
                            "/lib/udev/rules.d")
             "-Dsystemd=disabled")))
    (inputs
     (append (package-inputs pipewire)
             `(("bluez" ,bluez)
               ("jack" ,jack-2)
               ("pulseaudio" ,pulseaudio)
               ("vulkan-loader" ,vulkan-loader)
               ("vulkan-headers" ,vulkan-headers))))))
