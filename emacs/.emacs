;; Configure UI options
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(set-fringe-mode '(1 . 0))  ; Disable right-side fringe
(setq use-dialog-box nil)   ; Disable dialog boxes since they weren't working in Mac OSX

;; Set the font face based on platform
(cond
 ((string-equal system-type "cygwin") ; Cygwin on Microsoft Windows
    (set-face-attribute 'default nil :font "Consolas" :height 120))

 ((string-equal system-type "darwin")   ; Mac OS X
    ;(set-face-attribute 'default nil :font "Menlo" :height 150)
    (set-face-attribute 'default nil :font "Ubuntu Mono" :height 180))

 ((string-equal system-type "gnu/linux") ; linux
    (set-face-attribute 'default nil :font "Ubuntu Mono" :height 130)))

;; Configure line numbering
(global-linum-mode)
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

;; Show the date and time in the status bar
(setq display-time-day-and-date t)
(display-time)

;; TODO: remove this soon!
(setq package-check-signature nil)

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

;; Themes - http://emacsthemes.caisah.info/
;(use-package twilight-anti-bright-theme :ensure t)            ; Best so far
(use-package badger-theme :ensure t)                           ; Cool, but not my favorite
;(use-package dakrone-theme :ensure t)
;(load-theme 'tango-dark)                                      ; A little too mild
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (progn 
;;     (color-theme-sanityinc-tomorrow-eighties)))

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

(use-package org
  :ensure t
  :config
  (progn
    ;; Configure paths
    (setq org-directory "~/Notes")
    (setq org-default-notes-file (concat org-directory "/Inbox.org"))
    (setq org-blank-before-new-entry
	  '((heading . t)
	    (plain-list-item . auto)))

    ;; Configure the agenda
    (setq org-agenda-window-setup 'other-window)
    (setq org-agenda-files
	  '("~/Notes/Inbox.org" 
	    "~/Notes/Habits.org" 
	    "~/Notes/Personal.org" 
	    "~/Notes/Work.org" 
	    "~/Notes/Projects.org" 
	    "~/Notes/Reference/Emacs/OrgMode.org"))
    
    ;; Configure refile
    (setq org-refile-targets 
	  (quote ((nil :maxlevel . 9)
		  (org-agenda-files :maxlevel . 9))))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    
    ;; Configure TODO settings
    (setq org-log-into-drawer t)
    (setq org-log-reschedule 'time)
    (setq org-log-refile 'time)
    (setq org-todo-keywords
	  '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANC(c@)")))
    
    ;; Configure modules
    (setq org-modules 
	  '(org-bbdb org-crypt org-gnus org-habit org-bookmark org-drill org-eshell org-eval org-expiry org-learn org-notmuch org-man org-toc org-velocity org-docview org-info org-jsinfo org-irc org-mhe org-vm org-w3m org-wl))

    ;; Configure key bindings
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cb" 'org-iswitchb)
))

(use-package org-journal
  :ensure t
  :config
  (progn
    (setq org-journal-dir "~/Notes/Journal/")))
    
;; Check out the intro for more info: http://tuhdo.github.io/helm-intro.html
(use-package helm
  :ensure t
  :config
  (progn
    (require 'helm-config)
    (setq helm-buffers-fuzzy-matching t)
    (helm-mode 1)

    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)))

(use-package helm-company
  :ensure t
  :config
  (progn
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company)))

(use-package helm-package
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c C-p") 'helm-package)))

(use-package helm-themes
  :ensure t)

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
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    ;(global-rainbow-delimiters-mode)
    ))

;; TODO: Re-instate this ever?
;; (use-package flx-ido
;;   :ensure t
;;   :config
;;   (progn
;;     (ido-mode 1)
;;     (ido-everywhere 1)
;;     (flx-ido-mode 1)
;;     ;; disable ido faces to see flx highlights.
;;     (setq ido-enable-flex-matching t)
;;     (setq ido-use-faces nil)))

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
    (setq cider-lein-command "/usr/bin/lein")           ; Configure the path to lein
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

(use-package markdown-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 
       'auto-mode-alist 
       '("\\.html?\\'" . web-mode))))

;; Use 'M-x httpd-start' to start the web server
;; Use 'M-x impatient-mode' to start live reload for the current buffer
(use-package impatient-mode
  :ensure t)

(use-package csharp-mode
  :ensure t)

(use-package omnisharp
  :ensure t)

(use-package yasnippet
  :ensure t)

(use-package twittering-mode
  :ensure t
  :config
  (progn
    (setq twittering-icon-mode t)
    (twittering-enable-unread-status-notifier)))

;; Install ergoemacs mode and set desired config
;; (unless (package-installed-p 'ergoemacs-mode)
;;   (package-install 'ergoemacs-mode))
;; (require 'ergoemacs-mode)
;; (setq ergoemacs-theme "hardcore") ;; Uses Hardcore Ergoemacs keyboard theme
;; (setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
;; (ergoemacs-mode 1)

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

;; ---- Auto-generated variables ----
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols keep-place list match menu move-to-prompt netsplit networks noncommands notify notifications readonly ring stamp track)))
 ;; '(org-agenda-files
 ;;   (quote
 ;;    ("~/Notes/Inbox.org" "~/Notes/Habits.org" "~/Notes/Personal.org" "~/Notes/Work.org" "~/Notes/Projects.org" "~/Notes/Reference/Emacs/OrgMode.org")))
)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#F6F3E8" :background "#171717")))))
