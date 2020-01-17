;; The One True Desktop Environment

(specifications->manifest
 '("emacs"

   ;; Package Management
   "emacs-use-package"

   ;; Desktop Environment
   "emacs-exwm"
   "emacs-desktop-environment"
   "emacs-default-text-scale"
   "emacs-daemons"
   "emacs-pulseaudio-control"
   "emacs-symon"
   "emacs-alert"
   "emacs-guix"

   ;; Mode Line
   "emacs-doom-modeline"
   "emacs-smart-mode-line"
   "emacs-minions"

   ;; Themes
   "emacs-all-the-icons"
   "emacs-doom-themes"
   "emacs-spacegray-theme"

   ;; Authentication
   "emacs-pinentry"
   "pinentry-emacs"

   ;; Evil
   "emacs-evil"
   "emacs-evil-magit"
   "emacs-evil-collection"
   "emacs-evil-nerd-commenter"
   "emacs-undo-tree"

   ;; Key Bindings
   "emacs-hydra"
   "emacs-general"
   "emacs-which-key"

   ;; Git
   "emacs-forge"
   "emacs-magit"
   "emacs-magit-todos"
   "emacs-git-link"
   "emacs-git-gutter"

   ;; Quick Navigation
   "emacs-avy"
   "emacs-ace-window"
   "emacs-multiple-cursors" ;; Not in config yet

   ;; Completions and Search
   "emacs-ivy"
   "emacs-ivy-xref"
   "emacs-ivy-pass"
   "emacs-ivy-rich"
   "emacs-flx"
   "emacs-smex"
   "the-silver-searcher" ;; For counsel-projectile-ag
   "ripgrep" ;; For counsel-projectile-rg
   "emacs-counsel-projectile"

   ;; Dired
   "emacs-dired-hacks"
   "emacs-openwith"
   "emacs-all-the-icons-dired"

   ;; Org Mode
   "emacs-org"
   "emacs-org-contrib"
   "emacs-org-bullets"
   "emacs-org-pomodoro"
   "emacs-calfw"

   ;; IDE
   "emacs-flycheck"
   "emacs-projectile"
   "emacs-lsp-mode"
   "emacs-lsp-ui"
   "emacs-lsp-ivy"
   "emacs-yasnippet"
   "emacs-ivy-yasnippet"  ;; Not in config yet
   "emacs-yasnippet-snippets"

   ;; Lispy Languages
   "emacs-expand-region"
   "emacs-parinfer-mode"
   "emacs-smartparens"
   "emacs-cider"
   "emacs-rainbow-delimiters"

   ;; JavaScript / Typescript
   "emacs-typescript-mode"
   "emacs-prettier"
   "emacs-js2-mode"
   "emacs-web-mode"

   ;; Misc Languages
   "emacs-rust-mode"
   "emacs-helpful"
   "emacs-markdown-mode"
   "emacs-yaml-mode"
   "emacs-rainbow-mode"
   "emacs-know-your-http-well"

   ;; Writing
   "emacs-darkroom"
   ;;"emacs-writegood-mode"

   ;; Shell
   "emacs-eshell-z"
   "emacs-esh-autosuggest"
   "emacs-fish-completion"
   "emacs-multi-term"
   "emacs-xterm-color"
   "emacs-exec-path-from-shell"

   ;; Mail
   "emacs-mu4e-alert"

   ;; Misc Apps
   "emacs-mastodon"
   "emacs-elfeed"
   "emacs-emms"

   ;; Docker
   "emacs-docker"
   "emacs-docker-tramp"
   "emacs-dockerfile-mode"

   ;; Chat
   "emacs-emojify"
   "emacs-erc-image"
   "emacs-erc-hl-nicks"
   "emacs-tracking"
   "emacs-telega"

   ;; Finance
   "ledger"
   "emacs-ledger-mode"))
