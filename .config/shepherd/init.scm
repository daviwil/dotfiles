;; init.scm -- default shepherd configuration file.

(define gpg-agent
  (make <service>
    #:provides '(gpg-agent)
    #:respawn? #t
    #:start (make-system-constructor "gpg-connect-agent /bye")
    #:stop (make-system-destructor "gpgconf --kill gpg-agent")))

(define syncthing
  (make <service>
    #:provides '(syncthing)
    #:respawn? #t
    #:start (make-forkexec-constructor '("syncthing" "-no-browser"))
    #:stop  (make-kill-destructor)))

(define pulseaudio
  (make <service>
    #:provides '(pulseaudio)
    #:respawn? #t
    #:start (make-forkexec-constructor '("pulseaudio"))
    #:stop  (make-kill-destructor)))

;; Services known to shepherd:
;; Add new services (defined using 'make <service>') to shepherd here by
;; providing them as arguments to 'register-services'.
(register-services gpg-agent syncthing pulseaudio)

;; Send shepherd into the background
(action 'shepherd 'daemonize)

;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start '(gpg-agent syncthing pulseaudio))
