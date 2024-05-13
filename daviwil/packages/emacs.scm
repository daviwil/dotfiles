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

(define-public emacs-super-save
  (let ((commit "886b5518c8a8b4e1f5e59c332d5d80d95b61201d")
        (revision "0"))
    (package
      (name "emacs-super-save")
      (version (git-version "0.3.0" revision commit))
      (source
       (origin
         (uri (git-reference
               (url "https://github.com/bbatsov/super-save")
               (commit commit)))
         (method git-fetch)
         (sha256
          (base32 "1w62sd1vcn164y70rgwgys6a8q8mwzplkiwqiib8vjzqn87w0lqv"))
         (file-name (git-file-name name version))))
      (build-system emacs-build-system)
      (home-page "https://github.com/bbatsov/super-save")
      (synopsis "Emacs package for automatic saving of buffers")
      (description
       "super-save auto-saves your buffers, when certain events happen: you
switch between buffers, an Emacs frame loses focus, etc.  You can think of
it as an enhanced `auto-save-mode'")
      (license license:gpl3+))))
