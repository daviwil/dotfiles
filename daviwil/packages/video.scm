(define-module (daviwil packages video)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages video)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp))

(define-public obs-vaapi
  (package
    (name "obs-vaapi")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fzwoch/obs-vaapi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16yv1qsz3cdwdxivcwb3xivf8mzv14vvxrzzf2lz1ydkhcn8v49y"))))
    (arguments
     (list
      #:configure-flags #~(list (string-append "--prefix=" #$output "/lib/obs-plugins"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config cmake))
    (inputs (list obs gstreamer gst-plugins-base pciutils))
    (home-page "https://github.com/fzwoch/obs-vaapi")
    (synopsis "GStreamer-based VAAPI encoder for OBS.")
    (description "GStreamer-based VAAPI encoder for OBS.")
    (license license:gpl2+)))
