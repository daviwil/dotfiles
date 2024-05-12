(define-module (daviwil home-services emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix transformations)

  #:export (home-emacs-config-service-type))

(define transform
  (options->transformation
   ;; 2.3.0 does not include the `box :style none` fix
   '((with-commit . "emacs-doom-themes=3b2422b208d28e8734b300cd3cc6a7f4af5eba55"))))

(define (home-emacs-config-profile-service config)
  (map (lambda (package-name)
         (transform
          (specification->package+output package-name)))
       (list "emacs-next-pgtk"

             "emacs-tmr"
             "emacs-buffer-env"
             "emacs-beframe"

             "emacs-no-littering"

             "emacs-exwm"
             "emacs-desktop-environment"

             "emacs-god-mode"

             "emacs-popper"

             "emacs-mpv"

             "emacs-ement"
             "emacs-circe"

             "emacs-evil"
             "emacs-evil-collection"
             "emacs-undo-tree"

             "emacs-general"

             "emacs-lua-mode"
             "emacs-fennel-mode"

             "emacs-doom-themes"
             "emacs-spacegray-theme"

             "emacs-emojify"

             "emacs-mood-line"
             "emacs-minions"

             "emacs-alert"

             "emacs-super-save"

             "emacs-pinentry"
             "pinentry-emacs"

             "emacs-evil-nerd-commenter"

             "emacs-ws-butler"

             "emacs-hydra"

             "emacs-vertico"
             "emacs-corfu"
             "emacs-kind-icon"
             "emacs-orderless"
             "emacs-consult"
             "emacs-wgrep"
             "emacs-marginalia"
             "emacs-embark"

             "emacs-avy"
             "emacs-ace-window"

             "emacs-default-text-scale"
             "emacs-visual-fill-column"

             "emacs-password-store"
             "emacs-auth-source-pass"

             "emacs-dired-hacks"
             "emacs-all-the-icons-dired"

             "emacs-org"
             "emacs-org-modern"
             "emacs-org-pomodoro"
             "emacs-evil-org"
             "emacs-org-make-toc"
             "emacs-org-present"
             "emacs-org-roam"
             "emacs-org-appear"
             "emacs-org-ql"
             "emacs-htmlize"
             "emacs-denote"

             "emacs-magit"
             "emacs-magit-todos"

             "git"
             "git:send-email"

             "emacs-git-link"
             "emacs-git-gutter"
             "emacs-git-gutter-fringe"

             "emacs-project"
             "ripgrep" ;; For consult-ripgrep

             ;; TODO: Use the built-in Eglot for now to avoid problems
             ;; "emacs-eglot"

             "emacs-lispy"
             "emacs-lispyville"

             "emacs-sly"
             "emacs-sly-asdf"

             "emacs-js2-mode"
             "emacs-typescript-mode"
             "emacs-apheleia"

             "emacs-go-mode"

             "emacs-rust-mode"
             "emacs-zig-mode"

             "emacs-helpful"

             "emacs-geiser"

             "emacs-markdown-mode"

             "emacs-web-mode"
             "emacs-skewer-mode"

             "emacs-yaml-mode"

             "emacs-flycheck"

             "emacs-yasnippet"
             "emacs-yasnippet-snippets"

             "emacs-smartparens"

             "emacs-rainbow-delimiters"

             "emacs-rainbow-mode"

             "emacs-posframe"
             "emacs-keycast"

             "emacs-obs-websocket-el"

             "emacs-a"
             "emacs-request"

             "isync"
             "mu"
             "emacs-mu4e-alert"

             "ledger"
             ;; "hledger"
             "emacs-ledger-mode"

             "emacs-eat"
             "emacs-eshell-z"
             "emacs-esh-autosuggest"
             "emacs-xterm-color"
             "emacs-exec-path-from-shell"

             "emacs-pcmpl-args"

             "emacs-eshell-syntax-highlighting"

             "emacs-eshell-toggle"

             "emacs-vterm"

             "emacs-tracking"

             "emacs-telega"

             "emacs-elfeed"

             "emacs-elpher"

             "emacs-guix"

             "emacs-daemons"

             "emacs-pulseaudio-control"

             "emacs-docker"
             "emacs-docker-tramp"
             "emacs-dockerfile-mode")))

(define home-emacs-config-service-type
  (service-type (name 'home-emacs-config)
                (description "Applies my personal Emacs configuration.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-emacs-config-profile-service)))
                (default-value #f)))
