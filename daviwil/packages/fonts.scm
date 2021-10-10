(define-module (daviwil packages fonts)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:))

(define-public font-jost
  (package
    (name "font-jost")
    (version "3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/indestructible-type/Jost"
                    "/releases/download/" version "/Jost.zip"))
              (sha256
               (base32
                "1nb92imnk1g7c5nyra2q3k298ym8dp3hcxr1saxyj8mg6gxsqc0h"))))
    (build-system font-build-system)
    (home-page "https://indestructibletype.com/Jost.html")
    (synopsis "The Jost* font")
    (description "Jost* font.")
    (license license:silofl1.1)))
