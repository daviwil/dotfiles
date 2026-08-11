(define-module (daviwil home-services hermes)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages node)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (guix gexp)

  #:export (home-hermes-service-type))

;;; Hermes is Nous Research's self-improving agent.  There is no Guix package
;;; for it, but it is published to PyPI, so this service installs it with `uv`
;;; into ~/.local (the upstream-recommended path for non-Nix systems) and runs
;;; its messaging gateway as a Shepherd user service.
;;;
;;; The design goal is portability: drop `(service home-hermes-service-type)`
;;; into any Guix Home config, reconfigure, log in, and Hermes installs itself
;;; against the Guix-provided Python -- no manual `uv tool install`, no
;;; `pip`/venv juggling, and nothing that depends on this particular machine.

;; The package spec handed to `uv tool install`.  Edit the extras to taste --
;; e.g. "hermes-agent[all,anthropic]" to add the native Anthropic SDK, or pin a
;; version with "hermes-agent[all]==0.16.0" for reproducibility.
(define %hermes-agent-spec "hermes-agent[all]")

;; Where uv drops the `hermes` launcher.  Already on PATH via the common
;; profile-env-vars service ($HOME/.local/bin), so interactive use Just Works.
(define %hermes-bin "/.local/bin/hermes")

(define %hermes-log-dir "/.local/var/log")

(define (home-hermes-profile-service config)
  ;; Runtime dependencies.  The Python app itself lives in ~/.local; these are
  ;; the interpreter the venv is built against plus the external tools Hermes
  ;; shells out to.  Guix de-duplicates these against anything already in the
  ;; home profile.
  (list python      ; interpreter the uv venv is built against (GC root)
        uv          ; installs and updates the Hermes tool
        node        ; JS-based tools, MCP servers, fetching
        ripgrep     ; code search
        ffmpeg      ; audio / voice
        git
        openssh))

(define (home-hermes-activation config)
  ;; Make sure the log directory exists before Shepherd opens its log files.
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p (string-append (getenv "HOME") #$%hermes-log-dir))))

(define (home-hermes-shepherd-service config)
  (list
   ;; One-shot bootstrap: install Hermes via uv against the home-profile Python
   ;; so the venv references a GC-rooted store path.  Idempotent and
   ;; offline-safe -- it no-ops once the launcher exists.  Upgrade later with
   ;; `uv tool upgrade hermes-agent`.
   (shepherd-service
    (provision '(hermes-install))
    (one-shot? #t)
    (documentation "Install/update the hermes-agent CLI with uv.")
    (start
     #~(make-forkexec-constructor
        (list #$(file-append bash "/bin/bash") "-c"
              (string-append
               "set -e; "
               "export PATH=\"$HOME/.guix-home/profile/bin:$PATH\"; "
               "export UV_PYTHON_DOWNLOADS=never UV_PYTHON_PREFERENCE=only-system; "
               ;; Already installed?  Nothing to do (keeps logins fast/offline).
               "if [ -x \"$HOME" #$%hermes-bin "\" ]; then "
               "echo \"hermes-install: already present, skipping\"; exit 0; fi; "
               "echo \"hermes-install: installing " #$%hermes-agent-spec "\"; "
               "exec uv tool install "
               "--python \"$HOME/.guix-home/profile/bin/python3\" "
               "'" #$%hermes-agent-spec "'"))
        #:log-file (string-append (getenv "HOME")
                                  #$%hermes-log-dir "/hermes-install.log"))))

   ;; The messaging gateway (Telegram, Discord, WhatsApp, ...), run in the
   ;; foreground.  Left auto-start? #f because it needs `hermes setup` and
   ;; `hermes gateway setup` (provider keys + platform tokens) first; start it
   ;; with `herd start hermes-gateway`, then flip to #t once configured.
   (shepherd-service
    (provision '(hermes-gateway))
    (requirement '(hermes-install))
    (auto-start? #f)
    (documentation "Run the Hermes messaging gateway in the foreground.")
    (start
     #~(make-forkexec-constructor
        (list (string-append (getenv "HOME") #$%hermes-bin) "gateway" "run")
        #:log-file (string-append (getenv "HOME")
                                  #$%hermes-log-dir "/hermes-gateway.log")
        #:environment-variables
        (cons (string-append
               "PATH="
               (getenv "HOME") "/.guix-home/profile/bin:"
               (getenv "HOME") "/.local/bin:"
               "/run/current-system/profile/bin:"
               (or (getenv "PATH") ""))
              (default-environment-variables))))
    (stop #~(make-kill-destructor)))))

(define home-hermes-service-type
  (service-type (name 'home-hermes)
                (description
                 "Install the Hermes agent with uv and run its gateway as a \
Shepherd service.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-hermes-profile-service)
                       (service-extension
                        home-activation-service-type
                        home-hermes-activation)
                       (service-extension
                        home-shepherd-service-type
                        home-hermes-shepherd-service)))
                (default-value #f)))
