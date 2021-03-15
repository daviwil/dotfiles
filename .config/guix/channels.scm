;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

(list (channel
        (name 'channel-x)
        (url "file:///home/daviwil/Projects/Code/channel-x")
        (commit
          "1a8624799b84fb768215290fec1345467c495718"))
      (channel
        (name 'flat)
        (url "https://github.com/flatwhatson/guix-channel.git")
        (commit
          "302f8a4f7e56cb3b484de9fe86617a3aaf20098c")
        (introduction
          (make-channel-introduction
            "33f86a4b48205c0dc19d7c036c85393f0766f806"
            (openpgp-fingerprint
              "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
      (channel
        (name 'nonguix)
        (branch "add-sof-firmware")
        (url "file:///home/daviwil/Projects/Code/nonguix"))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit
          "b1cabedd28b92324259875fc52ca5d52d411a026")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

  ;; (cons* (channel
  ;;         (name 'channel-x)
  ;;         ;; (url "https://github.com/daviwil/channel-x"))
  ;;         (url "file:///home/daviwil/Projects/Code/channel-x"))
  ;;        (channel
  ;;         (name 'flat)
  ;;         (url "https://github.com/flatwhatson/guix-channel.git")
  ;;         (commit
  ;;          "302f8a4f7e56cb3b484de9fe86617a3aaf20098c")
  ;;         (introduction
  ;;          (make-channel-introduction
  ;;           "33f86a4b48205c0dc19d7c036c85393f0766f806"
  ;;           (openpgp-fingerprint
  ;;            "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
  ;;        (channel
  ;;         (name 'nonguix)
  ;;         (url "https://gitlab.com/nonguix/nonguix"))
  ;;        %default-channels)

  ;;(list (channel
  ;;        (name 'nonguix)
  ;;        (commit "c34fa8bfacdce5fa45b2a684c2b27309c09a9056")
  ;;        (url "https://gitlab.com/nonguix/nonguix"))
  ;;      (channel
  ;;        (name 'guix)
  ;;        (commit "190187326ad7516dd6728eed7bb6ef2d4f92897a")
  ;;        (url "https://git.savannah.gnu.org/git/guix.git")
  ;;        (introduction
  ;;          (make-channel-introduction
  ;;            "9edb3f66fd807b096b48283debdcddccfea34bad"
  ;;          (openpgp-fingerprint
  ;;            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

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
