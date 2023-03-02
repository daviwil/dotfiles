;; -*- lexical-binding: t; -*-

(require 'subr-x)

;;; -- System Identification -----

(defvar dw/is-termux
  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(defvar dw/is-guix-system (and (eq system-type 'gnu/linux)
                               (with-temp-buffer
                                 (insert-file-contents "/etc/os-release")
                                 (search-forward "ID=guix" nil t))
                               t))

(defvar dw/exwm-enabled (and (not dw/is-termux)
                             (eq window-system 'x)
                             (seq-contains command-line-args "--use-exwm")))

(defvar dw/mail-enabled (member system-name '("zerocool" "acidburn")))
(setq dw/mu4e-inbox-query nil)

;;; -- Basic Configuration Paths -----

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(setup (:pkg no-littering)
  (require 'no-littering))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;;; -- Native Compilation -----

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;;; -- Basic Emacs Settings -----

;; Thanks, but no thanks
(setq inhibit-startup-message t)

(unless dw/is-termux
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10))       ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; TODO: Mode this to another section
(setq-default fill-column 80)

;; Set up the visible bell
(setq visible-bell t)

(unless dw/is-termux
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a time
  (setq use-dialog-box nil)) ;; Disable dialog boxes since they weren't working in Mac OSX

(unless dw/is-termux
  (set-frame-parameter (selected-frame) 'alpha-background 93)
  (add-to-list 'default-frame-alist '(alpha-background 93))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Use UTF-8 by default
(set-default-coding-systems 'utf-8)

;;; -- Core Key Bindings and Packages ----

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-u") 'universal-argument)

(setup (:pkg undo-tree)
  (setq undo-tree-auto-save-history nil)
  (global-undo-tree-mode 1))

(setup (:pkg evil)
  ;; Pre-load configuration
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)

  ;; Activate the Evil
  (evil-mode 1)

  ;; Set Emacs state modes
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode))

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Clear the binding of C-k so that it doesn't conflict with Corfu
  (define-key evil-insert-state-map (kbd "C-k") nil)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (unless dw/is-termux
    (defun dw/dont-arrow-me-bro ()
      (interactive)
      (message "Arrow keys are bad, you know?"))

    ;; Disable arrow keys in normal and visual modes
    (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
    (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
    (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro))

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(setup (:pkg evil-collection)
  ;; Is this a bug in evil-collection?
  (setq evil-collection-company-use-tng nil)
  (:load-after evil
    (:option evil-collection-outline-bind-tab-p nil
             (remove evil-collection-mode-list) 'lispy
             (remove evil-collection-mode-list) 'org-present)
    (evil-collection-init)))

(setup (:pkg general)
  (general-evil-setup t)

  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c"))

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))


(setq large-file-warning-threshold nil)
(setq vc-follow-symlinks t)
(setq ad-redefinition-action 'accept)

;;; -- Appearance -----

(setup (:pkg doom-themes))
(unless dw/is-termux
  ;; TODO: Move this to a system setting
  (load-theme
   (pcase system-name
     ("acidburn" 'doom-ayu-dark)
     ("phantom" 'doom-molokai)
     (_ 'doom-palenight))
   t)

  (doom-themes-visual-bell-config))

;; TODO: Do I use this?  Is it needed?
(setup (:pkg default-text-scale)
  (default-text-scale-mode))

;; Set the font face based on platform
(pcase system-type
  ((or 'gnu/linux 'windows-nt 'cygwin)
   (set-face-attribute 'default nil
                       :font "JetBrains Mono"
                       :weight 'light
                       :height (dw/system-settings-get 'emacs/default-face-size)))
  ('darwin (set-face-attribute 'default nil :font "Fira Mono" :height 170)))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height (dw/system-settings-get 'emacs/fixed-face-size))

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    ;; :font "Cantarell"
                    :font "Iosevka Aile"
                    :height (dw/system-settings-get 'emacs/variable-face-size)
                    :weight 'light)

(setq display-time-format "%l:%M %p %b %d W%U"
      display-time-load-average-threshold 0.0)

;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!

;;; -- Mode Line -----

(setup (:pkg minions)
  (:hook-into doom-modeline-mode))

(defun dw/start-doom-modeline ()
  (require 'doom-modeline)

  ;; Start it
  (doom-modeline-mode 1)

  ;; Customize the default modeline
  (doom-modeline-def-modeline 'default
    '(bar window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state grip debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker))
  (doom-modeline-set-modeline 'default t))

(setup (:pkg doom-modeline)
  (add-hook 'after-init-hook #'dw/start-doom-modeline)
  (:option doom-modeline-height 15
           doom-modeline-bar-width 6
           doom-modeline-lsp t
           doom-modeline-github nil
           doom-modeline-mu4e nil
           doom-modeline-irc nil
           doom-modeline-minor-modes t
           doom-modeline-persp-name nil
           doom-modeline-buffer-file-name-style 'truncate-except-project
           doom-modeline-major-mode-icon nil)
  (custom-set-faces '(mode-line ((t (:height 0.85))))
                    '(mode-line-inactive ((t (:height 0.85))))))

;;; -- Timers -----

(setup (:pkg tmr))

(defun dw/tmr-mode-line ()
  (if (not (and (boundp 'tmr--timers)
                tmr--timers))
      ""
    (propertize (format " 🕐 %s: %s"
                        (tmr--format-remaining (car tmr--timers))
                        (tmr--timer-description (car tmr--timers)))
                'tab-bar '(:foreground "orange"))))

;;; -- Tab Bar Workspaces -----

(setup (:pkg tabspaces :straight t)
  (tabspaces-mode 1)
  (setq tabspaces-use-filtered-buffers-as-default t
        tabspaces-default-tab "Main"
        tabspaces-remove-to-default t
        tabspaces-include-buffers '("*scratch*")))

(with-eval-after-load 'consult
  ;; Hide full buffer list by default (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)

  ;; Set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name "Workspace Buffers"
          :narrow ?w
          :history 'buffer-name-history
          :category 'buffer
          :state #'consult--buffer-state
          :default t
          :items (lambda () (consult--buffer-query
                             :predicate #'tabspaces--local-buffer-p
                             :sort 'visibility
                             :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

(defun dw/switch-tab-buffer (&optional arg)
  (interactive "P")
  (cond
   ((and arg (> (car arg) 0)) (call-interactively #'consult-buffer))
   ((project-current) (call-interactively #'project-switch-to-buffer))
   (t (call-interactively #'consult-buffer))))

(global-set-key (kbd "C-M-j") #'consult-buffer)
(global-set-key (kbd "C-M-k") #'tab-bar-switch-to-tab)
(global-set-key (kbd "C-M-n") #'tab-bar-switch-to-next-tab)

(defun dw/exwm-workspace-icon ()
  (when dw/exwm-enabled
    (format " %s" (pcase exwm-workspace-current-index
                    (0 "💬")
                    (1 "💻")
                    (2 "🏄")
                    (3 "📬")
                    (4 "📸")))))

(defun dw/set-tab-bar-faces ()
  (let ((color (face-attribute 'doom-modeline-bar :background nil t)))
    (set-face-attribute 'tab-bar-tab nil :foreground nil :background nil :weight 'semi-bold :underline `(:color ,color) :inherit nil)
    (set-face-attribute 'tab-bar nil :font "Iosevka Aile" :foreground nil :inherit 'mode-line)))

(setq tab-bar-close-button-show nil
      tab-bar-format '(dw/exwm-workspace-icon
                       tab-bar-format-history
                       tab-bar-format-tabs-groups
                       tab-bar-separator
                       dw/tmr-mode-line
                       tab-bar-separator
                       tab-bar-format-align-right
                       tab-bar-format-global))

(with-eval-after-load 'doom-modeline
  (dw/set-tab-bar-faces)

  (add-to-list 'global-mode-string '(" " display-time-string))
  (add-to-list 'global-mode-string '(" " doom-modeline--battery-status))
  (add-to-list 'global-mode-string '(" " tracking-mode-line-buffers))

  (display-time-mode 1)
  (display-battery-mode 1)

  (setq tab-bar-show t)
  (tab-bar-mode 1)
  (tab-bar-rename-tab "Main"))

;;; -- Notifications -----

(setup (:pkg alert)
  (:option alert-default-style 'notifications))

;;; -- Editing Configuration -----

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(setup (:pkg evil-nerd-commenter)
  (:global "M-/" evilnc-comment-or-uncomment-lines))

(setup (:pkg ws-butler)
  (:hook-into text-mode prog-mode))

(setup (:pkg super-save)
  (:delay)
  (:when-loaded
    (super-save-mode +1)
    (setq super-save-auto-save-when-idle t)))

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(setup (:require paren)
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(setup (:pkg visual-fill-column)
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (:hook-into org-mode))

(setup (:pkg avy)
  (dw/leader-key-def
    "j"   '(:ignore t :which-key "jump")
    "jj"  '(avy-goto-char :which-key "jump to char")
    "jw"  '(avy-goto-word-0 :which-key "jump to word")
    "jl"  '(avy-goto-line :which-key "jump to line")))

;;; -- Window Management -----

(setup (:pkg ace-window)
  (:global "M-o" ace-window)
  (:option aw-scope 'frame
           aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
           aw-minibuffer-flag t)
  (ace-window-display-mode 1))

(setup winner
  (winner-mode)
  (define-key evil-window-map "u" 'winner-undo)
  (define-key evil-window-map "U" 'winner-redo))

;; (setq display-buffer-base-action
;;       '(display-buffer-reuse-mode-window
;;         display-buffer-reuse-window
;;         display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(defun dw/popper-window-height (window)
  (let (buffer-mode (with-current-buffer (window-buffer window)
                      major-mode))
    (pcase buffer-mode
      ('exwm-mode 40)
      (_ 15))))

(setup (:pkg popper
             :host github
             :repo "karthink/popper"
             :build (:not autoloads))
  (:global "C-M-'" popper-toggle-latest
           "M-'" popper-cycle
           "C-M-\"" popper-toggle-type)
  (:option popper-window-height 12
           ;; (popper-window-height
           ;; (lambda (window)
           ;;   (let ((buffer-mode (with-current-buffer (window-buffer window)
           ;;                        major-mode)))
           ;;     (message "BUFFER MODE: %s" buffer-mode)
           ;;     (pcase buffer-mode
           ;;       ('exwm-mode 40)
           ;;       ('helpful-mode 20)
           ;;       ('eshell-mode (progn (message "eshell!") 10))
           ;;       (_ 15)))))
           popper-reference-buffers '(eshell-mode
                                      vterm-mode
                                      geiser-repl-mode
                                      help-mode
                                      grep-mode
                                      helpful-mode
                                      compilation-mode))
  (require 'popper) ;; Needed because I disabled autoloads
  (popper-mode 1))

;;; -- Dired -----

(setup (:pkg all-the-icons-dired))
(setup (:pkg dired-single :straight t))
(setup (:pkg dired-ranger))
(setup (:pkg dired-collapse))

(defun dw/dired-mode-hook ()
  (interactive)
  ;; (dired-omit-mode 1)
  (dired-hide-details-mode 1)
  (unless (or dw/is-termux
              (s-equals? "/gnu/store/" (expand-file-name default-directory)))
    (all-the-icons-dired-mode 1))
  (hl-line-mode 1))

(setup dired
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook #'dw/dired-mode-hook)

  (unless dw/exwm-enabled
    (global-set-key (kbd "s-e") #'dired-jump))

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

(setup (:pkg dired-rainbow)
  (:load-after dired
   (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
   (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
   (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
   (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
   (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
   (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
   (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
   (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
   (dired-rainbow-define log "#c17d11" ("log"))
   (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
   (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
   (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
   (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
   (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
   (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
   (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
   (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
   (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
   (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
   (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")))

(setup (:pkg openwith)
  (unless dw/is-termux
    (require 'openwith)
    (setq openwith-associations
          (list
           (list (openwith-make-extension-regexp
                  '("mpg" "mpeg" "mp3" "mp4"
                    "avi" "wmv" "wav" "mov" "flv"
                    "ogm" "ogg" "mkv"))
                 "mpv"
                 '(file))
           (list (openwith-make-extension-regexp
                  '("xbm" "pbm" "pgm" "ppm" "pnm"
                    "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
                 ;; causing feh to be opened...
                 "feh"
                 '(file))
           (list (openwith-make-extension-regexp
                  '("pdf"))
                 "zathura"
                 '(file))))))

;;; -- World Clock -----

(setq display-time-world-list
  '(("Etc/UTC" "UTC")
    ("Europe/Athens" "Athens")
    ("America/Los_Angeles" "Seattle")
    ("America/Denver" "Denver")
    ("America/New_York" "New York")
    ("Pacific/Auckland" "Auckland")
    ("Asia/Shanghai" "Shanghai")
    ("Asia/Kolkata" "Hyderabad")))

(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

;;; -- Save Minibuffer History -----

(setup savehist
  (setq history-length 25)
  (savehist-mode 1))

;; Individual history elements can be configured separately
;;(put 'minibuffer-history 'history-length 25)
;;(put 'evil-ex-history 'history-length 50)
;;(put 'kill-ring 'history-length 25))

;;; -- Make Help More Helpful -----

(setup (:pkg helpful)
  (:option counsel-describe-function-function #'helpful-callable
           counsel-describe-variable-function #'helpful-variable)
  (:global [remap describe-function] helpful-function
           [remap describe-symbol] helpful-symbol
           [remap describe-variable] helpful-variable
           [remap describe-command] helpful-command
           [remap describe-key] helpful-key))

;; Load the info system for info files
(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

;;; -- Convenience Key Bindings ----

(defun dw/org-file-jump-to-heading (org-file heading-title)
  (interactive)
  (find-file (expand-file-name org-file))
  (goto-char (point-min))
  (search-forward (concat "* " heading-title))
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(defun dw/org-file-show-headings (org-file)
  (interactive)
  (find-file (expand-file-name org-file))
  (counsel-org-goto)
  (org-overview)
  (org-reveal)
  (org-show-subtree)
  (forward-line))

(dw/leader-key-def
  "fn" '((lambda () (interactive) (counsel-find-file "~/Notes/")) :which-key "notes")
  "fd"  '(:ignore t :which-key "dotfiles")
  "fdd" '((lambda () (interactive) (find-file "~/.dotfiles/Desktop.org")) :which-key "desktop")
  "fdc" '((lambda () (interactive) (find-file (expand-file-name (concat  "~/.dotfiles/daviwil/systems/" system-name ".scm")))) :which-key "system config")
  "fde" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/.emacs.d/init.el"))) :which-key  "edit config")
  "fdE" '((lambda () (interactive) (dw/org-file-show-headings "~/.dotfiles/Emacs.org")) :which-key "edit config")
  "fdm" '((lambda () (interactive) (find-file "~/.dotfiles/Mail.org")) :which-key "mail")
  "fdM" '((lambda () (interactive) (counsel-find-file "~/.dotfiles/.config/guix/manifests/")) :which-key "manifests")
  "fds" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" "Base Configuration")) :which-key "base system")
  "fdS" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" system-name)) :which-key "this system")
  "fdp" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Desktop.org" "Panel via Polybar")) :which-key "polybar")
  "fdw" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/Workflow.org"))) :which-key "workflow")
  "fdv" '((lambda () (interactive) (find-file "~/.dotfiles/.config/vimb/config")) :which-key "vimb"))

;;; -- Start the Daemon -----

(server-start)

(provide 'dw-core)
