(define-module (daviwil packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs-build)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-obs-websocket-el
  (let ((commit "2bdd39c45cd2d9ae08482f276752ebe45838189f")
        (revision "0"))
    (package
      (name "emacs-obs-websocket-el")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (uri (git-reference
               (url "https://github.com/obs-websocket-el/obs-websocket-el")
               (commit commit)))
         (method git-fetch)
         (sha256
          (base32 "1y2iq3s21vgvvinf9f6ciqpgawlg7zfs5mavif4bmif3gddmjv6a"))
         (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-websocket" ,emacs-websocket)))
      (home-page "https://github.com/obs-websocket-el/obs-websocket-el")
      (synopsis "Emacs package for remote control of OBS")
      (description
       "obs-websocket-el is a client for the obs-websocket plugin for OBS.
It enables you to remotely control your streaming or recording session from
Emacs!")
      (license license:gpl3+))))
