(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ; Things that are definitely needed
 ;'(frame-brackground-mode (quote dark))
 ;'(indent-tabs-mode nil)

 ;'(main-line-color1 "#191919")
 ;'(main-line-color2 "#111111")
 ;'(main-line-separator-style (quote chamfer))
 
 ;'(org-agenda-files (quote ("~/Dropbox/Tasks.org")))
 ;'(org-hide-leading-stars t)
 ;'(org-modules (quote (org-bbdb org-bibtex org-crypt org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-checklist org-git-link org-wl)))
 
 ;'(powerline-color1 "#191919")
 ;'(powerline-color2 "#111111")
 
 ;'(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 66 72 76 80 84 88 92 96 100 104 106 112 120)))
 ;'(tab-width 4)

 ;; '(vc-annotate-background monokai-bg)
 ;; '(vc-annotate-color-map (quote ((20 . monokai-fg-1) (40 . monokai-bg+2) (60 . monokai-red) (80 . monokai-red+1) (100 . monokai-orange) (120 . monokai-orange+1) (140 . monokai-green) (160 . monokai-green+1) (180 . monokai-yellow) (200 . monokai-yellow+1) (220 . monokai-blue) (240 . monokai-blue+1) (260 . monokai-purple) (280 . monokai-purple+1) (300 . monokai-cyan) (320 . monokai-cyan+1) (340 . monokai-magenta) (360 . monokai-magenta+1))))
 ;; '(vc-annotate-very-old-color monokai-magenta))
)

;; Configure UI options
(scroll-bar-mode -1)       ; Disable visible scrollbar
(tool-bar-mode -1)         ; Disable the toolbar
(set-fringe-mode '(1 . 0))  ; Disable right-side fringe
(setq use-dialog-box nil)   ; Disable dialog boxes since they weren't working in Mac OSX

;; Set the font face
;(set-face-attribute 'default nil :font "Menlo" :height 150)
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 180)

;; Configure line numbering
(global-linum-mode)
;(setq line-number-mode nil)
(setq linum-format " %3d ")

;; Save the current session upon exiting
(desktop-save-mode t)

;; Turn off backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Make scrolling less crazy
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Initialize MELPA
(require 'package)
(add-to-list
  'package-archives 
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(unless package-archive-contents 
  (package-refresh-contents))
(package-initialize)

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Add load paths
(add-to-list 'load-path "~/.emacs.d/scripts/")

;; Load color theme
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (progn 
;;     (color-theme-sanityinc-tomorrow-eighties)))

;(require 'twilight-anti-bright-theme)
;(require 'sanityinc-tomorrow-eighties)
;(require 'base16-tomorrow)

(use-package twilight-anti-bright-theme :ensure t)

(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (show-paren-mode 1)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (global-rainbow-delimiters-mode)))

(use-package evil 
  :ensure t
  :config
  (progn 
    (evil-mode 1)))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (progn
    ;; Use default hotkeys, but also add CMD-/
    (evilnc-default-hotkeys)
    (global-set-key (kbd "s-/") 'evilnc-comment-or-uncomment-lines)))

(use-package flx-ido
  :ensure t
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

(use-package company
  :ensure t
  :config
  (progn 
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package clojure-mode 
  :ensure t
  :config
  (progn
    (add-hook
      'clojure-mode-hook
      (lambda ()
	(eldoc-mode t)
	(smartparens-strict-mode)
	;(setq font-lock-verbose nil)
	;(global-set-key (kbd "C-c t") 'clojure-jump-between-tests-and-code)
	;(paredit-mode 1)
))))

(use-package cider
  :ensure t
  :config
  (progn 
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)     ; Use eldoc when cider is connected
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode) ; Use smartparens in the REPL
    (setq cider-prompt-save-file-on-load nil)                 ; Don't prompt to save file on load into REPL
    (setq nrepl-hide-special-buffers t)                       ; Hide special cider buffers
    (setq cider-auto-select-error-buffer nil)                 ; Don't automatically select error buffer when shown
    (setq cider-lein-command "/usr/local/bin/lein")           ; Configure the path to lein
))

(use-package magit
  :ensure t)

(use-package fsharp-mode 
  :ensure t
  :mode "\\.fs\\'"
  :interpreter "fsharp"
  :config
  (progn 
    (setq fsharp-ac-use-popup t)))

(use-package git-gutter
  :ensure t
  :config
  (progn 
    (global-git-gutter-mode +1)
    (setq git-gutter:modified-sign " ")
    (setq git-gutter:added-sign " ")
    (setq git-gutter:deleted-sign " ")
    (set-face-background 'git-gutter:modified "yellow")
    (set-face-background 'git-gutter:added "green")
    (set-face-background 'git-gutter:deleted "red")))

;; Install ergoemacs mode and set desired config
;; (unless (package-installed-p 'ergoemacs-mode)
;;   (package-install 'ergoemacs-mode))
;; (require 'ergoemacs-mode)
;; (setq ergoemacs-theme "hardcore") ;; Uses Hardcore Ergoemacs keyboard theme
;; (setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
;; (ergoemacs-mode 1)

;; org mode key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

;; Buffer switching hotkeys
(global-set-key "\M-[" 'previous-buffer)
(global-set-key "\M-]" 'next-user-buffer)

;; Cause ESC to cancel minibuffer actions
;; Ripped from http://stackoverflow.com/a/10166400
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Bind compile command to CMD-Return (ErgoEmacs changes Meta to Cmd)
;; TODO: Use recompile command if build command already set
;; See this page for ideas: http://www.emacswiki.org/emacs/CompileCommand
;; (defun run-game ()
;;   "Run RealTimeWebGame"
;;   (interactive)
;;   (shell-command "mono ~/Projects/Code/RealTimeWebGame/RealTimeWebGame/bin/Debug/RealTimeWebGame.exe /local"))

;; (global-set-key "\M-\r" 'compile)
;; ;(global-set-key (kbd "M-S-RET") 'run-game)
;; (global-set-key (kbd "M-m") 'run-game)

;; ;; Quick key for switching buffer
;; (global-set-key "\M-b" 'switch-to-buffer)

