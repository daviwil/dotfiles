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

;; Load org-mode and its settings
(load "org-mode.el")

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
  :ensure t
  :config
  (progn
    (global-set-key "\C-cgs" 'magit-status)
    (global-set-key "\C-cgd" 'magit-diff-unstaged)))

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

(use-package jabber
  :ensure t
  :config
  (progn
    ;(require 'jabber-autoloads)
    (setq jabber-account-list '(("david@daviwil.com/Emacs")))
    (setq jabber-invalid-certificate-servers '("daviwil.com"))
    (setq jabber-auto-reconnect t)))

(use-package smart-mode-line
  :ensure t
  :config 
  (progn
    (sml/setup)
    (setq rm-excluded-modes
	  ; These names must start with a space!
	  '(" GitGutter" " MRev" " company" " Helm" " Undo-Tree"))))

;; Install ergoemacs mode and set desired config
;; (unless (package-installed-p 'ergoemacs-mode)
;;   (package-install 'ergoemacs-mode))
;; (require 'ergoemacs-mode)
;; (setq ergoemacs-theme "hardcore") ;; Uses Hardcore Ergoemacs keyboard theme
;; (setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
;; (ergoemacs-mode 1)

(use-package emms
  :ensure t
  :config
  (progn
    (require 'emms-setup)
    (require 'emms-player-mpd)
    (emms-all)
    (emms-default-players)
    (add-to-list 'emms-info-functions 'emms-info-mpd)
    (add-to-list 'emms-player-list 'emms-player-mpd)
    (add-to-list 'emms-player-list 'emms-player-mplayer)
    (setq emms-player-mpd-music-directory "~/Music")
    (setq emms-mode-line-icon-color "white")
    (emms-mode-line-toggle)
    (emms-cache-set-from-mpd-all)
    (setq emms-source-file-default-directory "~/Music/")))

