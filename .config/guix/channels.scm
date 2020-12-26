;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

;; (cons* (channel
;;         (name 'nonguix)
;;         (commit "c34fa8bfacdce5fa45b2a684c2b27309c09a9056")
;;         (url "https://gitlab.com/nonguix/nonguix"))
;;        %default-channels)

(list (channel
        (name 'nonguix)
        (commit "c34fa8bfacdce5fa45b2a684c2b27309c09a9056")
        (url "https://gitlab.com/nonguix/nonguix"))
      (channel
        (name 'guix)
        (commit "190187326ad7516dd6728eed7bb6ef2d4f92897a")
        (url "https://git.savannah.gnu.org/git/guix.git")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
          (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

;; (list (channel
;;         (name 'nonguix)
;;         (url "https://gitlab.com/nonguix/nonguix"))
;;       (channel
;;         (name 'guix)
;;         (branch "fix-glu-pkg-config")
;;         (url "file:///home/daviwil/Projects/Code/guix")
;;         (introduction
;;           (make-channel-introduction
;;             "d06d5db885e4b8399e878708862fbe3a67f0592c"
;;             (openpgp-fingerprint
;;               "53C4 1E6E 41AA FE55 335A  CA5E 446A 2ED4 D940 BF14")))))
