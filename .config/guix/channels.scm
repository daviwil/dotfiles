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
        (url "file:///home/daviwil/Projects/Code/channel-x")
        (branch "master")
        (commit
          "025eca1931ca5c5489ec3f45a4af7ea558f1bec9")))
