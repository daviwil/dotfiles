;; Various developer tools that I use.  These might be split out into
;; platform-specific manifests at some point.

(specifications->manifest
 '(;; C/C++
   "gcc-toolchain"
   "make"
   "pkg-config"
   "texinfo"
   "llvm"
   "lld"
   "clang"
   "file"                               ; The 'file' command
   "elfutils"
   "go"

   ;; Python
   "python2"

   ;; Docker
   "docker-cli"

   ;; Java
   "icedtea"

   ;; Chibi
   "chibi-scheme"

   ;; Guile
   "guile"

   "virt-manager"))
   ;; "glibc" ;; For ldd
