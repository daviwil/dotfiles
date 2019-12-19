;; Various developer tools that I use.  These might be split out into
;; platform-specific manifests at some point.

(specifications->manifest
 '("node"
   "python2"
   "docker-cli"
   "gcc-toolchain"
   "curl"))
   ;; "glibc" ;; For ldd
