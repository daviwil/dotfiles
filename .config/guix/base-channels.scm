;; NOTE: This file is generated from ~/.dotfiles/System.org.  Please see commentary there.

(list (channel
        (name 'channel-x)
        (url "file:///home/daviwil/Projects/Code/channel-x"))
      (channel
        (name 'flat)
        (url "https://github.com/flatwhatson/guix-channel.git")
        (commit
          "7b8353ebbcf486e3344924d1cac0fa7ba47c371d")
        (introduction
          (make-channel-introduction
            "33f86a4b48205c0dc19d7c036c85393f0766f806"
            (openpgp-fingerprint
              "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix"))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        ;; (url "file:///home/daviwil/Projects/Code/guix"))
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
