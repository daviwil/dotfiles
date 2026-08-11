(define-module (daviwil home-services slate)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)

  #:export (home-slate-service-type))

;;; Runs a persistent Slate node so the workspace is reachable from other
;;; devices (phone, laptop) without leaving a terminal open.  The node dials
;;; OUT to Enclave, so nothing needs to be exposed inbound and there is no
;;; port to open.
;;;
;;; DELIBERATELY POINTS AT ~/.sigil/bin, NOT AT A BUILD DIRECTORY.  A repo
;;; checkout is a work surface -- workers build in it -- and a daemon whose
;;; binary lives there changes underneath you on the next build, or worse,
;;; execs a half-written artifact from a failed one.  ~/.sigil/bin is where
;;; `sigil app install` puts things, so this service works unchanged once
;;; Slate is tagged and installed through that mechanism.
;;;
;;; INTERIM, 2026-08-11: the binary currently at ~/.sigil/bin/slate-cli is a
;;; HAND-BUILT `--config dev` (optimize 0) build, placed there deliberately
;;; rather than through `sigil app install`.  Slate's `release` config is
;;; optimize 2, and every sigil toolchain from 0.18.3 to 0.18.5 carries a LICM
;;; miscompile that produces silent wrong code at optimize >= 1.  Once 0.18.6
;;; ships, relock Slate onto the fixed stdlib, tag it, and replace this with
;;; `sigil app install codeberg:sigil/slate --ref vX.Y.Z`.  Nothing about this
;;; service definition needs to change when that happens.

;;; LOCAL AND REMOTE ARE TWO PROCESSES TODAY.  `slate serve` (loopback, for a
;;; browser on this machine) and `slate node run` (Enclave, for a phone) are
;;; separate subcommands with separate top-level dispatch -- neither starts the
;;; other, so using Slate locally while the phone reaches it remotely means two
;;; running processes.  That is expected to work, since they claim different
;;; resources (a loopback port vs an outbound Enclave connection), but the two
;;; DO share on-disk state -- the origins store, the node identity, session
;;; state -- and no one has yet verified they are safe to run concurrently.
;;; Treat concurrent local+remote as unverified rather than supported until
;;; someone measures it.

(define %slate-bin "/.sigil/bin/slate-cli")

(define %slate-log-dir "/.local/var/log")

(define (home-slate-activation config)
  ;; Make sure the log directory exists before Shepherd opens its log file.
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p (string-append (getenv "HOME") #$%slate-log-dir))))

(define (home-slate-shepherd-service config)
  (list
   (shepherd-service
    (provision '(slate-node))
    (auto-start? #t)
    (respawn? #t)
    (documentation "Run the Slate node in the foreground for remote access.")
    ;; `node run`, NOT `serve`.  These are separate dispatch branches in
    ;; slate-cli and they do different jobs: `serve` binds a loopback listener
    ;; for a browser on THIS machine, while `node run` drives the Enclave poll
    ;; loop that carries remote PTY streams.  This service exists for the
    ;; second.  Running `slate` / `slate launch` interactively at the desk
    ;; starts the first, as a separate process -- see the note below.
    ;;
    ;; REQUIRES ONE-TIME SETUP: `slate node init --enclave <https-url>
    ;; --account <nick> --name <node-name>` must have been run, or `node run`
    ;; exits with "no Enclave identity".
    (start
     #~(make-forkexec-constructor
        (list (string-append (getenv "HOME") #$%slate-bin) "node" "run")
        ;; Shepherd captures stdout/stderr here.  NOTE: slate-cli has no
        ;; --log / --log-level of its own yet (courier and folio both do), so
        ;; this file is incidental output rather than diagnostics -- no
        ;; levels, no timestamps, no way to raise detail when something
        ;; misbehaves.  Wiring (sigil log) through slate-cli is tracked
        ;; separately and matters more than usual here, because this daemon is
        ;; used from a phone where there is no terminal to watch.
        #:log-file (string-append (getenv "HOME")
                                  #$%slate-log-dir "/slate-node.log")
        #:environment-variables
        (cons (string-append
               "PATH="
               (getenv "HOME") "/.guix-home/profile/bin:"
               (getenv "HOME") "/.sigil/bin:"
               (getenv "HOME") "/.local/bin:"
               "/run/current-system/profile/bin:"
               (or (getenv "PATH") ""))
              (default-environment-variables))))
    (stop #~(make-kill-destructor)))))

(define home-slate-service-type
  (service-type
   (name 'home-slate)
   (description "Persistent Slate node for remote workspace access.")
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-slate-shepherd-service)
          (service-extension home-activation-service-type
                             home-slate-activation)))
   (default-value #f)))
