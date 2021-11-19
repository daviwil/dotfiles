(define-module (daviwil systems acidburn)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (daviwil utils)
  #:use-module (daviwil systems common))

(home-environment
 (packages (gather-manifest-packages '(emacs desktop music video games)))
 (services common-home-services))
