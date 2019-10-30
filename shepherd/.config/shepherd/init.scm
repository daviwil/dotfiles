;; init.scm -- default shepherd configuration file.

(define syncthing
  (make <service>
    #:provides '(syncthing)
    #:respawn? #t
    #:start (make-forkexec-constructor
             '("/home/daviwil/.guix-profile/bin/syncthing" "-no-browser"))
    #:stop  (make-kill-destructor)))

;; Services known to shepherd:
;; Add new services (defined using 'make <service>') to shepherd here by
;; providing them as arguments to 'register-services'.
(register-services syncthing)

;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start '(syncthing))

;; Send shepherd into the background
(action 'shepherd 'daemonize)
