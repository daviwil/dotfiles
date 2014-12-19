;; Setup the config and scripts library paths
(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/scripts/")

;; Load customization settings from another file
(setq custom-file "~/.emacs.d/config/customize.el")
(load custom-file)

;; Turn on the elisp debugger
(setq debug-on-error t)

;; Thanks, but no thanks
(setq inhibit-startup-message t)

;; Configure UI options
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)          ; Disable the menu bar
(set-fringe-mode '(1 . 0))  ; Disable right-side fringe
(setq use-dialog-box nil)   ; Disable dialog boxes since they weren't working in Mac OSX

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

;; Set the font face based on platform
(on-platform-do
 ((windows cygwin) (set-face-attribute 'default nil :font "Consolas:antialias=subpixel" :height 120))
 (osx (set-face-attribute 'default nil :font "Ubuntu Mono" :height 180)) ;:height 150
 (linux (set-face-attribute 'default nil :font "Ubuntu Mono" :height 130)))

;; Set up the visible bell
(setq visible-bell t)

;; Configure line numbering
(global-linum-mode)
(column-number-mode)
(setq linum-format " %3d")

;; Turn off backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Make scrolling less crazy
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Show the date and time in the status bar
;(setq display-time-day-and-date nil)
;(display-time )

;; Show battery details in the status bar
;(display-battery-mode 1)
;(setq battery-update-interval 5)

;; Enable TRAMP
(require 'tramp)

;; Enable whole-file encryption
(require 'epa-file)

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

;; Themes - http://emacsthemes.caisah.info/
;(use-package twilight-anti-bright-theme :ensure t) 
(use-package badger-theme :ensure t)
;(use-package dakrone-theme :ensure t)
;(load-theme 'tango-dark)
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :config
;;   (progn 
;;     (color-theme-sanityinc-tomorrow-eighties)))

;; Configure duplicate buffer name uniqueness
;; Names will look like "foo:a" and "foo:b"
;; TODO: This doesn't seem to be working correctly yet...
(require 'uniquify)
(setq 
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; Start winner-mode (C-c left / right for undo/redo)
(use-package winner
  :ensure t
  :config
  (winner-mode 1))

;; Load packages and their settings
(load "misc-packages.el")

;; TODO: Find a more elegant way to accomplish this
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

;; Set up SLIME for StumpWM
;(load (expand-file-name "~/quicklisp/slime-helper.el"))
;(setq inferior-lisp-program "sbcl")

