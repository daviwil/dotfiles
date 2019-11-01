(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix"))
       %default-channels)

;; The following channel list can be used when testing patches
;; to services from a local clone of the Guix repo.  Just make
;; sure to commit the changes to a branch and refer to that
;; branch by name in the 'guix channel entry below:

;; (list (channel
;;         (name 'nonguix)
;;         (url "https://gitlab.com/nonguix/nonguix"))
;;       (channel
;;         (name 'guix)
;;         (branch "fix-xfce-power-manager-polkit")
;;         (url "file:///home/daviwil/Projects/Code/guix")))
