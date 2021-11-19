(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (commit
          "93b2ce1197d2dcda81b900a89545f8f6f00ac570")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (commit
          "cebe5197f387e070da0f501f88a1a2f40c63634a"))
      (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (commit
          "4acf4597cf57f5c9635dd441610c1a2576e3a910")
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      (channel
        (name 'flat)
        (url "https://github.com/flatwhatson/guix-channel.git")
        (commit
          "29901dc2a2b5650974f1f8be05ddc51e438a8cde")
        (introduction
          (make-channel-introduction
            "33f86a4b48205c0dc19d7c036c85393f0766f806"
            (openpgp-fingerprint
              "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
      (channel
        (name 'channel-x)
        (url "file:///home/daviwil/Projects/Code/channel-x")
        (commit
          "025eca1931ca5c5489ec3f45a4af7ea558f1bec9")))
