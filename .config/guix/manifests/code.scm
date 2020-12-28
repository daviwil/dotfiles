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

   ;; Python
   "python2"

   ;; Docker
   "docker-cli"

   ;; Java
   "icedtea"

   ;; SDL
   "glu"
   "glfw"
   "sdl2"
   "sdl2-image"
   "sdl2-mixer"
   "sdl2-gfx"
   "sdl2-ttf"

   "curl"
   "virt-manager"))
   ;; "glibc" ;; For ldd
