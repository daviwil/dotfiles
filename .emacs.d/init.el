;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(load-file "~/.emacs.d/per-system-settings.el")

(require 'subr-x)
(setq dw/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

;; TODO: Need a more accurate check for this!
(setq dw/is-guix-system (eq window-system 'x))

;; Keep transient cruft out of ~/.emacs.d/
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Fix an issue accessing the ELPA archive in Termux
(when dw/is-termux
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(package-initialize)
;(unless package-archive-contents
;  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (or (package-installed-p 'use-package)
            dw/is-guix-system)
   (package-install 'use-package))
(require 'use-package)

;; Uncomment this to get a reading on packages that get loaded at startup
;;(setq use-package-verbose t)

;; On non-Guix systems, "ensure" packages by default
(setq use-package-always-ensure (not dw/is-guix-system))

;; Add my elisp path to load-path
(push "~/.emacs.d/elisp" load-path)

;; Helper function for changing OS platform keywords to system-type strings
(defun platform-keyword-to-string (platform-keyword)
  (cond
   ((eq platform-keyword 'windows) "windows-nt")
   ((eq platform-keyword 'cygwin) "cygwin")
   ((eq platform-keyword 'osx) "darwin")
   ((eq platform-keyword 'linux) "gnu/linux")))

;; Define a macro that runs an elisp expression only on a particular platform
(defmacro on-platform-do (&rest platform-expressions)
  `(cond
    ,@(mapcar
       (lambda (platform-expr)
     (let ((keyword (nth 0 platform-expr))
           (expr (nth 1 platform-expr)))
       `(,(if (listp keyword)
         `(or
           ,@(mapcar
              (lambda (kw) `(string-equal system-type ,(platform-keyword-to-string kw)))
              keyword))
          `(string-equal system-type ,(platform-keyword-to-string keyword)))
          ,expr)))
       platform-expressions)))

(server-start)

(setq dw/exwm-enabled (and (not dw/is-termux)
                           (eq window-system 'x)
                           (seq-contains command-line-args "--use-exwm")))

(when dw/exwm-enabled
  (load-file "~/.dotfiles/.emacs.d/exwm.el"))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun dw/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))

(defun dw/dont-arrow-me-bro ()
  (interactive)
  (message "Arrow keys are bad, you know?"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (add-hook 'evil-mode-hook 'dw/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Disable arrow keys in normal and visual modes
  (define-key evil-normal-state-map (kbd "<left>") 'dw/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<right>") 'dw/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<down>") 'dw/dont-arrow-me-bro)
  (define-key evil-normal-state-map (kbd "<up>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<left>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<right>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<down>") 'dw/dont-arrow-me-bro)
  (evil-global-set-key 'motion (kbd "<up>") 'dw/dont-arrow-me-bro)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c"))

(use-package use-package-chords
  :disabled
  :config (key-chord-mode 1))

;; Thanks, but no thanks
(setq inhibit-startup-message t)

(unless dw/is-termux
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10))       ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

(unless dw/is-termux
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1)) ;; keyboard scroll one line at a timesetq use-dialog-box nil) ; Disable dialog boxes since they weren't working in Mac OSX

(unless dw/is-termux
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                erc-mode-hook
                term-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                neotree-mode-hook
                telega-chat-mode-hook
                telega-root-mode-hook
                telega-webpage-mode-hook
                dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq large-file-warning-threshold nil)

(setq vc-follow-symlinks t)

(setq ad-redefinition-action 'accept)

(use-package spacegray-theme :defer t)
(use-package twilight-anti-bright-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package doom-themes :defer t)
(unless dw/is-termux
  (load-theme 'spacegray t))

;; Set the font face based on platform
(on-platform-do
 ((windows cygwin) (set-face-attribute 'default nil :font "Fira Mono:antialias=subpixel" :height 130))
  (osx (set-face-attribute 'default nil :font "Fira Mono" :height 170))
  ;(linux (set-face-attribute 'default nil :font "Fira Code" :height 120)))
  (linux (set-face-attribute 'default nil :font "Fira Code Retina" :height 120)))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 135 :weight 'regular)

(defun dw/replace-unicode-font-mapping (block-name old-font new-font)
  (let* ((block-idx (cl-position-if
                         (lambda (i) (string-equal (car i) block-name))
                         unicode-fonts-block-font-mapping))
         (block-fonts (cadr (nth block-idx unicode-fonts-block-font-mapping)))
         (updated-block (cl-substitute new-font old-font block-fonts :test 'string-equal)))
    (setf (cdr (nth block-idx unicode-fonts-block-font-mapping))
          `(,updated-block))))

(use-package unicode-fonts
  :ensure t
  :if (not dw/is-termux)
  :custom
  (unicode-fonts-skip-font-groups '(low-quality-glyphs))
  :config
  ;; Fix the font mappings to use the right emoji font
  (mapcar
    (lambda (block-name)
      (dw/replace-unicode-font-mapping block-name "Apple Color Emoji" "Noto Color Emoji"))
    '("Dingbats"
      "Emoticons"
      "Miscellaneous Symbols and Pictographs"
      "Transport and Map Symbols"))
  (unicode-fonts-setup))

(use-package emojify
  :hook (erc-mode . emojify-mode)
  :commands emojify-mode)

(setq display-time-format "%l:%M %p %b %y"
      display-time-default-load-average nil)

(use-package diminish)

(use-package smart-mode-line
  :disabled
  :if dw/is-termux
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful)  ; Respect the theme colors
  (setq sml/mode-width 'right
      sml/name-width 60)

  (setq-default mode-line-format
  `("%e"
      ,(when dw/exwm-enabled
          '(:eval (format "[%d] " exwm-workspace-current-index)))
      mode-line-front-space
      evil-mode-line-tag
      mode-line-mule-info
      mode-line-client
      mode-line-modified
      mode-line-remote
      mode-line-frame-identification
      mode-line-buffer-identification
      sml/pos-id-separator
      (vc-mode vc-mode)
      " "
      ;mode-line-position
      sml/pre-modes-separator
      mode-line-modes
      " "
      mode-line-misc-info))

  (setq rm-excluded-modes
    (mapconcat
      'identity
      ; These names must start with a space!
      '(" GitGutter" " MRev" " company"
      " Helm" " Undo-Tree" " Projectile.*" " Z" " Ind"
      " Org-Agenda.*" " ElDoc" " SP/s" " cider.*")
      "\\|")))

;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!

(use-package minions
  :hook (doom-modeline-mode . minions-mode)
  :custom
  (minions-mode-line-lighter ""))

(use-package doom-modeline
  :after eshell     ;; Make sure it gets hooked after eshell
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 1.00))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

(use-package super-save
  :ensure t
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(global-auto-revert-mode 1)

(dw/leader-key-def
  "t"  '(:ignore t :which-key "toggles")
  "tw" 'whitespace-mode
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package paren
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(setq display-time-world-list
  '(("America/Los_Angeles" "Seattle")
    ("America/New_York" "New York")
    ("Europe/Athens" "Athens")
    ("Pacific/Auckland" "Auckland")
    ("Asia/Shanghai" "Shanghai")))
(setq display-time-world-time-format "%a, %d %b %I:%M %p %Z")

(unless dw/is-termux
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; This configuration seems to work but might need tweaking
(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
(global-whitespace-mode)

(use-package parinfer
  :hook ((clojure-mode . parinfer-mode)
         (emacs-lisp-mode . parinfer-mode)
         (common-lisp-mode . parinfer-mode)
         (scheme-mode . parinfer-mode)
         (lisp-mode . parinfer-mode))
  :config
  (setq parinfer-extensions
      '(defaults       ; should be included.
        pretty-parens  ; different paren styles for different modes.
        evil           ; If you use Evil.
        smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
        smart-yank)))  ; Yank behavior depend on mode.

(dw/leader-key-def
  "tp" 'parinfer-toggle-mode)

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
  "fde" '((lambda () (interactive) (find-file (expand-file-name "~/.dotfiles/Emacs.org"))) :which-key "edit config")
  "fdE" '((lambda () (interactive) (dw/org-file-show-headings "~/.dotfiles/Emacs.org")) :which-key "edit config")
  "fdm" '((lambda () (interactive) (counsel-find-file "~/.dotfiles/.config/guix/manifests/")) :which-key "manifests")
  "fds" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" "Base Configuration")) :which-key "base system")
  "fdS" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Systems.org" system-name)) :which-key "this system")
  "fdp" '((lambda () (interactive) (dw/org-file-jump-to-heading "~/.dotfiles/Desktop.org" "Panel via Polybar")) :which-key "polybar")
  "fdv" '((lambda () (interactive) (find-file "~/.dotfiles/.config/vimb/config")) :which-key "vimb"))

(use-package hydra
  :defer 1)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)

  ;; Use different regex strategies per completion command
  (push '(completion-at-point . ivy--regex-fuzzy) ivy-re-builders-alist) ;; This doesn't seem to work...
  (push '(swiper . ivy--regex-ignore-order) ivy-re-builders-alist)
  (push '(counsel-M-x . ivy--regex-ignore-order) ivy-re-builders-alist)

  ;; Set minibuffer height for different commands
  (setf (alist-get 'counsel-projectile-ag ivy-height-alist) 15)
  (setf (alist-get 'counsel-projectile-rg ivy-height-alist) 15)
  (setf (alist-get 'swiper ivy-height-alist) 15)
  (setf (alist-get 'counsel-switch-buffer ivy-height-alist) 7))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich--display-transformers-list
        (plist-put ivy-rich--display-transformers-list
                   'ivy-switch-buffer
                   '(:columns
                     ((ivy-rich-candidate (:width 40))
                      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
                      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
                      (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
                      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
                     :predicate
                     (lambda (cand)
                       (if-let ((buffer (get-buffer cand)))
                         ;; Don't mess with EXWM buffers
                         (with-current-buffer buffer
                           (not (derived-mode-p 'exwm-mode)))))))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package flx  ;; Improves sorting for fuzzy-matched results
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package smex ;; Adds M-x recent command sorting for counsel-M-x
  :defer 1
  :after counsel)

(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

(dw/leader-key-def
  "r"   '(ivy-resume :which-key "ivy resume")
  "f"   '(:ignore t :which-key "files")
  "ff"  '(counsel-find-file :which-key "open file")
  "C-f" 'counsel-find-file
  "fr"  '(counsel-recentf :which-key "recent files")
  "fR"  '(revert-buffer :which-key "revert file")
  "fj"  '(counsel-file-jump :which-key "jump to file"))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(dw/leader-key-def
  "j"   '(:ignore t :which-key "jump")
  "jj"  '(avy-goto-char :which-key "jump to char")
  "jw"  '(avy-goto-word-0 :which-key "jump to word")
  "jl"  '(avy-goto-line :which-key "jump to line"))

(defun dw/ignore-non-vimb-buffers (buffer-name)
  (if-let ((buf (get-buffer buffer-name)))
    (when buf
      (with-current-buffer buf
        (not (and (derived-mode-p 'exwm-mode)
                  (string-equal exwm-class-name "Vimb")))))))

(defun dw/switch-to-browser-buffer ()
  (interactive)
  (let ((ivy-use-virtual-buffers nil)
        (ivy-ignore-buffers (append ivy-ignore-buffers '(dw/ignore-non-vimb-buffers))))
    (counsel-switch-buffer)))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)
(global-set-key (kbd "C-M-k") 'dw/switch-to-browser-buffer)

(dw/leader-key-def
  "b"   '(:ignore t :which-key "buffers")
  "bb"  'counsel-switch-buffer
  "bd"  'bury-buffer)

(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(winner-mode)
(define-key evil-window-map "u" 'winner-undo)

;; (defun dw/center-buffer-with-margins ()
;;   (let ((margin-size (/ (- (frame-width) 80) 3)))
;;     (set-window-margins nil margin-size margin-size)))

(defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-mode
  :defer t
  :hook (org-mode . dw/org-mode-visual-fill))

(use-package expand-region
  :if (not dw/is-termux)
  :bind (("M-[" . er/expand-region)
         ("C-(" . er/mark-outside-pairs)))

(use-package ivy-pass
  :commands ivy-pass
  :config
  (setq password-store-password-length 12))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(dw/leader-key-def
  "ap" '(:ignore t :which-key "pass")
  "app" 'ivy-pass
  "api" 'password-store-insert
  "apg" 'password-store-generate)

(use-package dired
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
    (lambda ()
    (interactive)
    (dired-collapse)))

  (add-hook 'dired-mode-hook
    (lambda ()
    (interactive)
    (dired-omit-mode 1)
    (unless (or dw/is-termux
                (s-equals? "/gnu/store/" (expand-file-name default-directory)))
      (all-the-icons-dired-mode 1))
    (hl-line-mode 1)))

  (use-package dired-rainbow
    :defer 2
    :config
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
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

  (use-package dired-single
    :ensure t
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

(defun dw/dired-link (path)
  (lexical-let ((target path))
    (lambda () (interactive) (message "Path: %s" target) (dired target))))

(dw/leader-key-def
  "d"   '(:ignore t :which-key "dired")
  "dd"  '(dired :which-key "Here")
  "dh"  `(,(dw/dired-link "~") :which-key "Home")
  "dn"  `(,(dw/dired-link "~/Notes") :which-key "Notes")
  "do"  `(,(dw/dired-link "~/Downloads") :which-key "Downloads")
  "dp"  `(,(dw/dired-link "~/Pictures") :which-key "Pictures")
  "dv"  `(,(dw/dired-link "~/Videos") :which-key "Videos")
  "d."  `(,(dw/dired-link "~/.dotfiles") :which-key "dotfiles")
  "de"  `(,(dw/dired-link "~/.emacs.d") :which-key ".emacs.d"))

(use-package openwith
  :if (not dw/is-termux)
  :config
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
             '(file))))
  (openwith-mode 1))

;; TODO: Mode this to another section
(setq-default fill-column 80)

;; Turn on indentation and auto-fill mode for Org files
(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :defer t
  ;; :ensure org-plus-contrib
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 1)

  (setq org-modules
    '(org-crypt
        org-habit
        org-bookmark
        org-eshell
        org-irc))

  (setq org-refile-targets '((nil :maxlevel . 3)
                            (org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (ledger . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  ;; NOTE: Subsequent sections are still part of this use-package block!

;; Since we don't want to disable org-confirm-babel-evaluate all
;; of the time, do it around the after-save-hook
(defun dw/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dw/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode)))

(use-package org-bullets
  :if (not dw/is-termux)
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(dolist (face '((org-level-1 . 1.4)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.15)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; TODO: Others to consider
;; '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-property-value ((t (:inherit fixed-pitch))) t)
;; '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
;; '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(setq org-directory
  (if dw/is-termux
      "~/storage/shared/Notes"
      "~/Notes"))

(defun dw/org-path (path)
  (expand-file-name path org-directory))

(setq org-journal-dir (dw/org-path "Journal/"))

(defun dw/get-todays-journal-file-name ()
  "Gets the journal file name for today's date"
  (interactive)
  (let* ((journal-file-name
           (expand-file-name
             (format-time-string "%Y/%Y-%2m-%B.org")
             org-journal-dir))
         (journal-year-dir (file-name-directory journal-file-name)))
    (if (not (file-directory-p journal-year-dir))
      (make-directory journal-year-dir))
    journal-file-name))

(setq org-default-notes-file (dw/org-path "Projects.org"))

(setq org-agenda-files
  (list
    (dw/org-path "Habits.org")
    (dw/org-path "Work.org")
    (dw/org-path "AutoRest.org")
    (dw/org-path "Calendar/Personal.org")
    (dw/org-path "Calendar/Work.org")
    (dw/org-path "Projects.org")))
    ;(dw/get-todays-journal-file-name)))

(setq org-agenda-window-setup 'other-window)
(setq org-agenda-span 'day)
(setq org-stuck-projects '("+LEVEL=2/TODO" ("NEXT") nil ""))
(setq org-agenda-start-with-log-mode t)

;; Configure custom agenda views
(setq org-agenda-custom-commands
  '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "PROC" ((org-agenda-overriding-header "Process Tasks")))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))
      ;; (todo "TODO"
      ;;   ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
      ;;    (org-agenda-files `(,dw/org-inbox-path))
      ;;    (org-agenda-text-search-extra-files nil)))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("p" "Active Projects"
     ((agenda "")
      (todo "ACTIVE"
        ((org-agenda-overriding-header "Active Projects")
         (org-agenda-max-todos 5)
         (org-agenda-files org-agenda-files)))))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))

    ;; Projects on hold
    ("h" tags-todo "+LEVEL=2/+HOLD"
     ((org-agenda-overriding-header "On-hold Projects")
      (org-agenda-files org-agenda-files)))

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))))

;; Configure common tags
(setq org-tag-alist
  '((:startgroup)
     ; Put mutually exclusive tags here
     (:endgroup)
     ("@errand" . ?E)
     ("@home" . ?H)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("planning" . ?p)
     ("publish" . ?P)
     ("batch" . ?b)
     ("note" . ?n)
     ("idea" . ?i)
     ("thinking" . ?t)
     ("recurring" . ?r)))

;; Configure task state change tag triggers
;; (setq org-todo-state-tags-triggers
;;   (quote (("CANC" ("cancelled" . t))
;;           ("WAIT" ("waiting" . t))
;;           ("HOLD" ("waiting") ("onhold" . t))
;;           (done ("waiting") ("onhold"))
;;           ("TODO" ("waiting") ("cancelled") ("onhold"))
;;           ("DONE" ("waiting") ("cancelled") ("onhold")))))

;; Configure TODO settings
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-datetree-add-timestamp 'inactive)
(setq org-habit-graph-column 60)
(setq org-fontify-whole-heading-line t)
(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "PROC" "|" "DONE(d!)")
    (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")
    (sequence "GOAL(g)" "|" "ACHIEVED(v)" "MAINTAIN(m)")))

(use-package org-journal
  :defer t
  :ensure t  ;; Not in Guix yet
  :custom
  (org-journal-file-type 'daily)
  (org-journal-date-format "%B %d, %Y - %A")
  (org-journal-dir "~/Notes/Journal/")
  (org-journal-time-format "%-l:%M %p - ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-enable-agenda-integration t))

(defun dw/read-file-as-string (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(setq org-capture-templates
  `(("t" "Tasks / Projects")
    ("tt" "Task" entry (file+olp ,(dw/org-path "Projects.org") "Projects" "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
    ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
    ("tp" "New Project" entry (file+olp ,(dw/org-path "Projects.org") "Projects" "Inbox")
         "* PLAN %?\n  %U\n  %a\n  %i" :empty-lines 1)

    ("j" "Journal Entries")
    ("jj" "Journal" entry
         (file+olp+datetree ,(dw/get-todays-journal-file-name))
         ;"\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
         :clock-in :clock-resume
         :empty-lines 1)
    ("jm" "Meeting" entry
         (file+olp+datetree ,(dw/get-todays-journal-file-name))
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
    ("jt" "Thinking" entry
         (file+olp+datetree ,(dw/get-todays-journal-file-name))
         "\n* %<%I:%M %p> - %^{Topic} :thoughts:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)
    ("jc" "Clocked Entry Notes" entry
         (file+olp+datetree ,(dw/get-todays-journal-file-name))
         "* %<%I:%M %p> - %K :notes:\n\n%?"
         :empty-lines 1)
    ("jg" "Clocked General Task" entry
         (file+olp+datetree ,(dw/get-todays-journal-file-name))
         "* %<%I:%M %p> - %^{Task description} %^g\n\n%?"
         :clock-in :clock-resume
         :empty-lines 1)

    ("w" "Workflows")
    ("we" "Checking Email" entry (file+olp+datetree ,(dw/get-todays-journal-file-name))
         "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

    ("m" "Metrics Capture")
    ("mw" "Weight" table-line (file+headline "~/Notes/Metrics.org" "Weight")
     "| %U | %^{Weight} | %^{Notes} |" :kill-buffer)
    ("mp" "Blood Pressure" table-line (file+headline "~/Notes/Metrics.org" "Blood Pressure")
     "| %U | %^{Systolic} | %^{Diastolic} | %^{Notes}" :kill-buffer)))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))

(use-package org-pomodoro
  :after org
  :commands org-pomodoro
  :config
  (setq org-pomodoro-start-sound "~/.emacs.d/sounds/focus_bell.wav")
  (setq org-pomodoro-short-break-sound "~/.emacs.d/sounds/three_beeps.wav")
  (setq org-pomodoro-long-break-sound "~/.emacs.d/sounds/three_beeps.wav")
  (setq org-pomodoro-finished-sound "~/.emacs.d/sounds/meditation_bell.wav")
  (dw/leader-key-def
    "op"  '(org-pomodoro :which-key "pomodoro")))

(require 'org-protocol)

(defun dw/counsel-rg-org-files ()
  (interactive)
  (counsel-rg "" "~/Notes" nil "Search Notes: "))

(use-package evil-org
  :after org
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-mode)
         (evil-org-mode . (lambda () (evil-org-set-key-theme '(navigation todo insert textobjects additional)))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(dw/leader-key-def
  "o"   '(:ignore t :which-key "org mode")

  "oi"  '(:ignore t :which-key "insert")
  "oil" '(org-insert-link :which-key "insert link")

  "on"  '(org-toggle-narrow-to-subtree :which-key "toggle narrow")

  "os"  '(dw/counsel-rg-org-files :which-key "search notes")

  "oa"  '(org-agenda :which-key "status")
  "oc"  '(org-capture t :which-key "capture")
  "ox"  '(org-export-dispatch t :which-key "export"))

;; This ends the use-package org-mode block
)

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

;; (use-package org-gcal
  ;;   :after org
  ;;   :config

  ;;   (setq org-gcal-client-id (password-store-get "API/Google/daviwil-emacs-id")
  ;;         org-gcal-client-secret (password-store-get "API/Google/daviwil-emacs-secret")
  ;;         org-gcal-file-alist `(("daviwil@github.com" . ,(dw/org-path "Calendar.org"))
  ;;                               (,(password-store-get "Misc/Calendars/GitHub/AtomTeam") . ,(dw/org-path "Calendar.org"))
  ;;                              )))

  ;; (dw/leader-key-def
  ;;   "ac"  '(:ignore t :which-key "calendar")
  ;;   "acs" '(org-gcal-fetch :which-key "sync"))

(use-package org-caldav
  :disabled
  :defer t
  :init
  (setq org-caldav-url "https://caldav.fastmail.com/dav/calendars/user/daviwil@fastmail.fm/"
        org-caldav-inbox nil
        org-caldav-calendar-id nil
        org-caldav-calendars
         '((:calendar-id "fe098bfb-0726-4e10-bff2-55f8278c8a56"
            :inbox "~/Notes/Calendar/Personal.org")
           (:calendar-id "8f150437-cc57-4ba0-9200-d1d98389e2e4"
            :inbox "~/Notes/Calendar/Work.org"))
        org-caldav-delete-org-entries 'always
        org-caldav-delete-calendar-entries 'never))

;; (use-package org-wild-notifier
;;   :after org
;;   :config
;;   ; Make sure we receive notifications for non-TODO events
;;   ; like those synced from Google Calendar
;;   (setq org-wild-notifier-keyword-whitelist nil)
;;   (setq org-wild-notifier-notification-title "Agenda Reminder")
;;   (setq org-wild-notifier-alert-time 15)
;;   (org-wild-notifier-mode))

(defun dw/org-start-presentation ()
  (interactive)
  (org-tree-slide-mode 1)
  (setq text-scale-mode-amount 3)
  (text-scale-mode 1))

(defun dw/org-end-presentation ()
  (interactive)
  (text-scale-mode 0)
  (org-tree-slide-mode 0))

(use-package org-tree-slide
  :defer t
  :after org
  :commands org-tree-slide-mode
  :config
  (evil-define-key 'normal org-tree-slide-mode-map
    (kbd "q") 'dw/org-end-presentation
    (kbd "C-j") 'org-tree-slide-move-next-tree
    (kbd "C-k") 'org-tree-slide-move-previous-tree)
  (setq org-tree-slide-slide-in-effect nil
        org-tree-slide-activate-message "Presentation started."
        org-tree-slide-deactivate-message "Presentation ended."
        org-tree-slide-header t))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;; Add a super-convenient global binding for magit-status since
;; I use it 8 million times a day
(global-set-key (kbd "C-M-;") 'magit-status)

(dw/leader-key-def
  "g"   '(:ignore t :which-key "git")
  "gs"  'magit-status
  "gd"  'magit-diff-unstaged
  "gc"  'magit-branch-or-checkout
  "gl"   '(:ignore t :which-key "log")
  "glc" 'magit-log-current
  "glf" 'magit-log-buffer-file
  "gb"  'magit-branch
  "gP"  'magit-push-current
  "gp"  'magit-pull-branch
  "gf"  'magit-fetch
  "gF"  'magit-fetch-all
  "gr"  'magit-rebase)

(use-package forge
  :disabled)

(use-package magit-todos
  :defer t)

(use-package git-link
  :commands git-link
  :config
  (setq git-link-open-in-browser t)
  (dw/leader-key-def
    "gL"  'git-link))

(use-package git-gutter
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (unless dw/is-termux
    (require 'git-gutter-fringe)
    (set-face-foreground 'git-gutter-fr:added "LightGreen")
    (fringe-helper-define 'git-gutter-fr:added nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
    (fringe-helper-define 'git-gutter-fr:modified nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX")

    (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
    (fringe-helper-define 'git-gutter-fr:deleted nil
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      ".........."
      ".........."
      "XXXXXXXXXX"
      "XXXXXXXXXX"
      "XXXXXXXXXX"))

  ;; These characters are used in terminal mode
  (setq git-gutter:modified-sign "≡")
  (setq git-gutter:added-sign "≡")
  (setq git-gutter:deleted-sign "≡")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-global-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile)

(dw/leader-key-def
  "pf"  'counsel-projectile-find-file
  "ps"  'counsel-projectile-switch-project
  "pF"  'counsel-projectile-rg
  "pp"  'counsel-projectile
  "pc"  'projectile-compile-project
  "pd"  'projectile-dired)

(dir-locals-set-class-variables 'Atom
  `((nil . ((projectile-project-name . "Atom")
            (projectile-project-compilation-dir . nil)
            (projectile-project-compilation-cmd . "script/build")))))

(dir-locals-set-directory-class (expand-file-name "~/Projects/Code/atom") 'Atom)

(use-package ivy-xref
  :init (if (< emacs-major-version 27)
          (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package lsp-mode
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
         ("TAB" . completion-at-point)))

(dw/leader-key-def
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-show))

;; (use-package dap-mode
;;   :ensure t
;;   :hook (lsp-mode . dap-mode)
;;   :config
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (require 'dap-node)
;;   (dap-node-setup)

;;   (dap-register-debug-template "Node: Attach"
;;     (list :type "node"
;;           :cwd nil
;;           :request "attach"
;;           :program nil
;;           :port 9229
;;           :name "Node::Run")))

(use-package cider
  :mode "\\.clj[sc]?\\'"
  :config
  (evil-collection-cider-setup))

(use-package sly
  :disabled
  :mode "\\.lisp\\'")

(use-package slime
  :disabled
  :mode "\\.lisp\\'")

(use-package nvm
  :defer t)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :ensure t
  :defer t)

(use-package fsharp-mode
  :mode ".fs[iylx]?\\'")

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(dw/leader-key-def
  "e"   '(:ignore t :which-key "eval")
  "eb"  '(eval-buffer :which-key "eval buffer"))

(dw/leader-key-def
  :keymaps '(visual)
  "er" '(eval-region :which-key "eval region"))

(setq geiser-default-implementation 'guile)

(use-package markdown-mode
  :pin melpa-stable
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun dw/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun dw/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :defer t
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

;; TODO: Figure out how to query for 'done' bugs
(defun dw/debbugs-guix-patches ()
  (interactive)
  (debbugs-gnu '("serious" "important" "normal") "guix-patches" nil t))

(use-package know-your-http-well
  :defer t)

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0))

(defun dw/enter-focus-mode ()
  (interactive)
  (darkroom-mode 1)
  (display-line-numbers-mode 0))

(defun dw/leave-focus-mode ()
  (interactive)
  (darkroom-mode 0)
  (display-line-numbers-mode 1))

(defun dw/toggle-focus-mode ()
  (interactive)
  (if (symbol-value darkroom-mode)
    (dw/leave-focus-mode)
    (dw/enter-focus-mode)))

(dw/leader-key-def
  "tf" '(dw/toggle-focus-mode :which-key "focus mode"))

(dw/leader-key-def
  "a"  '(:ignore t :which-key "apps"))

;; TODO: Move this check elsewhere
(setq dw/mail-enabled (string-equal system-name "zerocool"))

(use-package mu4e
  :if (and (eq system-type 'gnu/linux) dw/mail-enabled)
  :config
  ;; After building/installing mu4e the .el files are here:
  ;;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e") ;; On Fedora
  ;;(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e") ;; On Manjaro / Arch

  (require 'org-mu4e)
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Refresh mail using offlineimap every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-maildir "~/Mail")

  ;; Set up contexts for email accounts
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Fastmail"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
            :vars '(
                    (user-full-name . "David Wilson")
                    (user-mail-address . "david@daviwil.com")
                    (mu4e-sent-folder . "/Fastmail/Sent Items")
                    (mu4e-trash-folder . "/Fastmail/Trash")
                    (mu4e-drafts-folder . "/Fastmail/Drafts")
                    (mu4e-refile-folder . "/Fastmail/Archive")
                    (mu4e-sent-messages-behavior . sent)
                    ))
          ,(make-mu4e-context
            :name "Personal"
            :match-func (lambda (msg) (when msg
                                        (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
            :vars '(
                    (mu4e-sent-folder . "/Personal/Sent")
                    (mu4e-trash-folder . "/Personal/Deleted")
                    (mu4e-refile-folder . "/Personal/Archive")
                    ))
          ))
  (setq mu4e-context-policy 'pick-first)

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses 't)

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Sending mail
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type  'ssl)

  ;; Signing messages (use mml-secure-sign-pgpmime)
  (setq mml-secure-openpgp-signers '("53C41E6E41AAFE55335ACA5E446A2ED4D940BF14"))

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"       . ?i)
           ("/Sent Mail"   . ?s)
           ("/Trash"       . ?t)
           ("/All Mail"    . ?a)))

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name "All Inboxes"
                :query "maildir:/Fastmail/INBOX OR maildir:/Personal/Inbox"
                :key ?i))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq dw/mu4e-inbox-query
        "(maildir:/Personal/Inbox OR maildir:/Fastmail/INBOX) AND flag:unread")

  (defun dw/go-to-inbox ()
    (interactive)
    (mu4e-headers-search dw/mu4e-inbox-query))

  (dw/leader-key-def
    "m"  '(:ignore t :which-key "mail")
    "mm" 'mu4e
    "mi" 'dw/go-to-inbox
    "ms" 'mu4e-update-mail-and-index)

  ;; Start mu4e in the background so that it syncs mail periodically
  (run-at-time "10 sec" nil
               (lambda ()
                 (let ((current-prefix-arg '(4)))
                   (call-interactively 'mu4e)))))

(use-package mu4e-alert
  :after mu4e
  :config
  ;; Use Emacs' built-in notifier
  (mu4e-alert-set-default-style 'notifications)

  ;; Show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query dw/mu4e-inbox-query)

  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))

(use-package calfw
  :disabled
  :commands cfw:open-org-calendar
  :config
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)

  (use-package calfw-org
    :config
    (setq cfw:org-agenda-schedule-args '(:timestamp))))

(dw/leader-key-def
  "cc"  '(cfw:open-org-calendar :which-key "calendar"))

(use-package ledger-mode
  :mode "\\.lgr\\'"
  :bind (:map ledger-mode-map
              ("TAB" . completion-at-point)))

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun dw/get-current-package-version ()
  (interactive)
  (let ((package-json-file (concat (eshell/pwd) "/package.json")))
    (when (file-exists-p package-json-file)
      (let* ((package-json-contents (read-file package-json-file))
             (package-json (ignore-errors (json-parse-string package-json-contents))))
        (when package-json
          (ignore-errors (gethash "version" package-json)))))))

(defun dw/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun dw/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'dw/map-line-to-status-char status-lines)))))

(defun dw/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
      (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.
(defun dw/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (package-version (dw/get-current-package-version)))
    (concat
     "\n"
     (propertize (system-name) 'face `(:foreground "#62aeed"))
     (propertize " ॐ " 'face `(:foreground "white"))
     (propertize (dw/get-prompt-path) 'face `(:foreground "#82cfd3"))
     (when current-branch
       (concat
        (propertize " • " 'face `(:foreground "white"))
        (propertize (concat " " current-branch) 'face `(:foreground "#c475f0"))))
     (when package-version
       (concat
        (propertize " @ " 'face `(:foreground "white"))
        (propertize package-version 'face `(:foreground "#e8a206"))))
     (propertize " • " 'face `(:foreground "white"))
     (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground "#5a5b7f"))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\nλ" 'face `(:foreground "#aece4a")))
     (propertize " " 'face `(:foreground "white")))))

(unless dw/is-termux
  (add-hook 'eshell-banner-load-hook
            '(lambda ()
               (setq eshell-banner-message
                     (concat "\n" (propertize " " 'display (create-image "~/.dotfiles/.emacs.d/images/flux_banner.png" 'png nil :scale 0.2 :align-to "center")) "\n\n")))))

(defun dw/eshell-configure ()
  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)

  (use-package xterm-color)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            '(lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            '(lambda () (setenv "TERM" "dumb")))

  ;; Use Ivy to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      'dw/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil))

(use-package eshell
  :hook (eshell-first-time-mode . dw/eshell-configure)
  :init
  (setq eshell-directory-name "~/.emacs.d/eshell/"))

(use-package eshell-z
  :hook ((eshell-mode . (lambda () (require 'eshell-z)))
         (eshell-z-change-dir .  (lambda () (eshell/pushd (eshell/pwd))))))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(dw/leader-key-def
  "SPC" 'eshell)

(with-eval-after-load 'esh-opt
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-visual-commands '("htop" "zsh" "vim")))

;; (use-package eterm-256color
;;   :hook (term-mode . eterm-256color-mode))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

(use-package vterm
  :commands vterm)

(use-package multi-term
  :commands multi-term-next
  :config
  (setq term-buffer-maximum-size 10000)
  (setq term-scroll-to-bottom-on-output t)
  (add-hook 'term-mode-hook
      (lambda ()
        (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
        (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next)))))

(dw/leader-key-def
  "C-SPC" 'multi-term-next)

;; Don't let ediff break EXWM, keep it in one frame
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package tracking
  :defer t
  :config
  (setq tracking-faces-priorities '(all-the-icons-pink
                                    all-the-icons-lgreen
                                    all-the-icons-lblue))
  (setq tracking-frame-behavior nil))

;; Add faces for specific people in the modeline.  There must
;; be a better way to do this.
(defun dw/around-tracking-add-buffer (original-func buffer &optional faces)
  (let* ((name (buffer-name buffer))
         (face (cond ((s-contains? "Maria" name) '(all-the-icons-pink))
                     ((s-contains? "Alex " name) '(all-the-icons-lgreen))
                     ((s-contains? "Steve" name) '(all-the-icons-lblue))))
         (result (apply original-func buffer (list face))))
    (dw/update-polybar-telegram)
    result))

(defun dw/after-tracking-remove-buffer (buffer)
  (dw/update-polybar-telegram))

(advice-add 'tracking-add-buffer :around #'dw/around-tracking-add-buffer)
(advice-add 'tracking-remove-buffer :after #'dw/after-tracking-remove-buffer)
(advice-remove 'tracking-remove-buffer #'dw/around-tracking-remove-buffer)

;; Advise exwm-workspace-switch so that we can more reliably clear tracking buffers
;; NOTE: This is a hack and I hate it.  It'd be great to find a better solution.
(defun dw/before-exwm-workspace-switch (frame-or-index &optional force)
  (when (fboundp 'tracking-remove-visible-buffers)
    (when (eq exwm-workspace-current-index 0)
      (tracking-remove-visible-buffers))))

(advice-add 'exwm-workspace-switch :before #'dw/before-exwm-workspace-switch)

(use-package telega
  :commands telega
  :config
  (setq telega-user-use-avatars nil
        telega-use-tracking-for '(any pin unread)
        telega-chat-use-markdown-formatting t
        telega-emoji-use-images t
        telega-completing-read-function #'ivy-completing-read
        telega-msg-rainbow-title nil
        telega-chat-fill-column 75))

(defun dw/on-erc-track-list-changed ()
  (dolist (buffer erc-modified-channels-alist)
    (tracking-add-buffer (car buffer))))

(use-package erc-hl-nicks
  :after erc)

(use-package erc-image
  :after erc)

(use-package erc
  :commands erc
  :hook (erc-track-list-changed . dw/on-erc-track-list-changed)
  :config
  (setq
      erc-nick "daviwil"
      erc-user-full-name "David Wilson"
      erc-prompt-for-nickserv-password nil
      erc-auto-query 'bury
      erc-join-buffer 'bury
      erc-interpret-mirc-color t
      erc-rename-buffers t
      erc-lurker-hide-list '("JOIN" "PART" "QUIT")
      erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE")
      erc-track-enable-keybindings nil
      erc-track-visibility nil ; Only use the selected frame for visibility
      erc-fill-column 80
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      erc-track-exclude '("#twitter_daviwil")
      erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#guix"))
      erc-quit-reason (lambda (s) (or s "Fading out..."))
      erc-modules
      '(autoaway autojoin button completion fill irccontrols keep-place
          list match menu move-to-prompt netsplit networks noncommands
          readonly ring stamp track image hl-nicks))

  (add-hook 'erc-join-hook 'bitlbee-identify)
  (defun bitlbee-identify ()
    "If we're on the bitlbee server, send the identify command to the &bitlbee channel."
    (when (and (string= "127.0.0.1" erc-session-server)
               (string= "&bitlbee" (buffer-name)))
      (erc-message "PRIVMSG" (format "%s identify %s"
                                     (erc-default-target)
                                     (password-store-get "IRC/Bitlbee"))))))

(defun dw/connect-irc ()
  (interactive)
  (erc-tls
     :server "chat.freenode.net" :port 7000
     :nick "daviwil" :password (password-store-get "IRC/Freenode")))
  ;; (erc
  ;;    :server "127.0.0.1" :port 6667
  ;;    :nick "daviwil" :password (password-store-get "IRC/Bitlbee")))

(dw/ctrl-c-keys
  "c"  '(:ignore t :which-key "chat")
  "cb" 'erc-switch-to-buffer
  "cc" 'dw/connect-irc
  "ca" 'erc-track-switch-buffer)

(use-package mastodon
  :defer t
  :config
  (setq mastodon-instance-url "https://mastodon.social"))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
    '("https://nullprogram.com/feed/"
      "https://ambrevar.xyz/atom.xml"
      "https://guix.gnu.org/feeds/blog.atom"
      "https://valdyas.org/fading/feed/"
      "https://www.reddit.com/r/emacs/.rss")))

(use-package emms
  :commands emms
  :config
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (emms-mode-line-disable)
  (setq emms-source-file-default-directory "~/Music/")
  (dw/leader-key-def
    "am"  '(:ignore t :which-key "media")
    "amp" '(emms-pause :which-key "play / pause")
    "amf" '(emms-play-file :which-key "play file")))

(use-package guix
  :defer t)

(dw/leader-key-def
  "G"  '(:ignore t :which-key "Guix")
  "Gg" '(guix :which-key "Guix")
  "Gi" '(guix-installed-user-packages :which-key "user packages")
  "GI" '(guix-installed-system-packages :which-key "system packages")
  "Gp" '(guix-packages-by-name :which-key "search packages")
  "GP" '(guix-pull :which-key "pull"))

(use-package daemons
  :commands daemons)

(use-package pulseaudio-control
  :commands pulseaudio-control-select-sink-by-name
  :config
  (setq pulseaudio-control-pactl-path "/run/current-system/profile/bin/pactl"))

(defun dw/bluetooth-connect-q30 ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 11:14:00:00:1E:1A"))

(defun dw/bluetooth-connect-qc35 ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- connect 04:52:C7:5E:5C:A8"))

(defun dw/bluetooth-disconnect ()
  (interactive)
  (start-process-shell-command "bluetoothctl" nil "bluetoothctl -- disconnect"))

(use-package proced
  :commands proced
  :config
  (setq proced-auto-update-interval 1)
  (add-hook 'proced-mode-hook
            (lambda ()
              (proced-toggle-auto-update 1))))

(use-package docker
  :commands docker)

(use-package docker-tramp
  :defer t
  :after docker)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
