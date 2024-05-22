(define-module (daviwil packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-obs-websocket-el
  (let ((commit "ce6be2a417705098e26c76122eff2a0261f57d42")
        (revision "0"))
    (package
      (name "emacs-obs-websocket-el")
      (version (git-version "0.1" revision commit))
      (source
       (origin
         (uri (git-reference
               (url "https://github.com/sachac/obs-websocket-el")
               (commit commit)))
         (method git-fetch)
         (sha256
          (base32 "0fdjpxc75jk9cxgg66p24z1n5w9zzqbrci5mlqam31a2caip9bmx"))
         (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (propagated-inputs `(("emacs-websocket" ,emacs-websocket)))
      (home-page "https://github.com/sachac/obs-websocket-el")
      (synopsis "Emacs package for remote control of OBS")
      (description
       "obs-websocket-el is a client for the obs-websocket plugin for OBS.
It enables you to remotely control your streaming or recording session from
Emacs!")
      (license license:gpl3+))))
