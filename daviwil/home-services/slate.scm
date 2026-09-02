(define-module (daviwil home-services slate)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)

  #:export (home-slate-service-type))

;;; Runs the persistent Slate companion without leaving a terminal open.
;;; `slate node run` always brings up its loopback listener and optionally
;;; dials OUT to Enclave using the existing enrollment.  A service value may
;;; name an explicit private-network address for the direct node listener;
;;; #f keeps the listener disabled.
;;;
;;; The executable is owned by `slate node install --persistent`, not a build
;;; checkout.  The stable launcher follows Slate's atomically updated `active`
;;; marker, so this service definition does not change between releases.

(define %slate-bin "/.local/bin/slate")

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
    (documentation "Run the Slate local and Enclave node companion.")
    (start
     #~(make-forkexec-constructor
        (append
         (list (string-append (getenv "HOME") #$%slate-bin) "node" "run")
         (if #$config (list "--direct-host" #$config) '()))
        ;; Shepherd captures stdout/stderr here.  This file IS diagnostics as
        ;; of 2026-08-25: every node-side operation (open/write/stat, folder
        ;; enumeration, terminal open and session lifecycle, and every RPC the
        ;; dispatcher handles including ones it refuses) logs request, result
        ;; and duration through (sigil log), with a correlation id tying each
        ;; request's lines together.  Before that this file was 18 lines over
        ;; 12 days and the operation paths carried zero log calls, which is
        ;; why a node could sit dead for four hours reporting healthy.
        #:log-file (string-append (getenv "HOME")
                                  #$%slate-log-dir "/slate-node.log")
        ;; SIGIL_LOG_LEVEL is the volume control, and it is an ENV VAR rather
        ;; than a flag on purpose: when something misbehaves you want to raise
        ;; detail without editing a service definition.  Operations log at
        ;; debug and errors at warn, so `info` (the default when unset) gives
        ;; you failures only -- set `debug` to see what the node is actually
        ;; doing, `trace` to include terminal byte I/O.
        ;;
        ;; NOTE: per-module filtering does NOT work in the pinned sigil-log
        ;; 0.16.0 (documented but a no-op, t-618434), so this global level is
        ;; the only control there is.  `trace` is loud.
        #:environment-variables
        (cons* (string-append
                "PATH="
                (getenv "HOME") "/.guix-home/profile/bin:"
                (getenv "HOME") "/.sigil/bin:"
                (getenv "HOME") "/.local/bin:"
                "/run/current-system/profile/bin:"
                (or (getenv "PATH") ""))
               "SIGIL_LOG_LEVEL=debug"
               ;; Explicit child contract consumed by GET /node/status.  The
               ;; node must not guess its manager from PPID/process names.
               "SLATE_SUPERVISION=shepherd"
               "SLATE_AUTOSTART=configured"
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
