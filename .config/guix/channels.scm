(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "ec74eef890d01a82128ffc9d0188a886c710c8e8")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "d4f3c4b57e05fd3286722ed44b6500093c632258"))
      (channel
        (name 'channel-x)
        (url "https://github.com/daviwil/channel-x")
        (branch "master")
        (commit
          "0c442c2479fc86fed791e401663d964c999732bf")))
