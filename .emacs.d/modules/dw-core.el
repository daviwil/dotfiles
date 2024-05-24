;; -*- lexical-binding: t; -*-

(defvar dw/mail-enabled (member system-name '("zerocool" "acidburn")))
(setq dw/mu4e-inbox-query nil)

;;; -- Basic Configuration Paths -----

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering
  :demand t
  :config
  ;; Set the custom-file to a file that won't be tracked by Git
  (setq custom-file (if (boundp 'server-socket-dir)
                        (expand-file-name "custom.el" server-socket-dir)
                      (no-littering-expand-etc-file-name "custom.el")))
  (when (file-exists-p custom-file)
    (load custom-file t))

  ;; Don't litter project folders with backup files
  (let ((backup-dir (no-littering-expand-var-file-name "backup/")))
    (make-directory backup-dir t)
    (setq backup-directory-alist
          `(("\\`/tmp/" . nil)
            ("\\`/dev/shm/" . nil)
            ("." . ,backup-dir))))

  (setq auto-save-default nil)

  ;; Tidy up auto-save files
  (setq auto-save-default nil)
  (let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
    (make-directory auto-save-dir t)
    (setq auto-save-file-name-transforms
          `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
             ,(concat temporary-file-directory "\\2") t)
            ("\\`\\(/tmp\\|/dev/shm\\)\\([^/]*/\\)*\\(.*\\)\\'" "\\3")
            ("." ,auto-save-dir t)))))

;;; -- Native Compilation -----

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;;; -- Basic Emacs Settings -----

(setq inhibit-startup-message t)

(unless dw/is-termux
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10))       ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

(setq-default fill-column 80)

(setq visible-bell t)

(unless dw/is-termux
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't)       ;; scroll window under mouse
  (setq scroll-step 1)                     ;; keyboard scroll one line at a time
  (setq use-dialog-box nil)) ;; Disable dialog boxes since they weren't working in Mac OSX

(unless dw/is-termux
  (set-frame-parameter (selected-frame) 'alpha-background 93)
  (add-to-list 'default-frame-alist '(alpha-background . 93))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Use UTF-8 by default
(set-default-coding-systems 'utf-8)

;;; -- Core Key Bindings and Packages ----

(repeat-mode 1)

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

(use-package doom-themes
  :config
  (unless dw/is-termux
    ;; TODO: Move this to a system setting
    (load-theme
     (pcase system-name
       ("acidburn" 'doom-ayu-dark)
       ("phantom" 'doom-molokai)
       (_ 'doom-palenight))
     t)

    (doom-themes-visual-bell-config)))

;; TODO: Do I use this?  Is it needed?
(use-package default-text-scale
  :config
  (default-text-scale-mode))

;; Set the font face based on platform
(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :weight 'normal
                    :height (dw/system-settings-get 'emacs/default-face-size))

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
                    :font "JetBrains Mono"
                    :weight 'light
                    :height (dw/system-settings-get 'emacs/fixed-face-size))

(defvar dw/org-heading-font "Iosevka Aile")

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font dw/variable-pitch-font
                    :height 120
                    :weight 'normal)

(setq display-time-format "%l:%M %p %b %d W%U"
      display-time-load-average-threshold 0.0)

(defun dw/activate-theme-tweaks (_theme)
  ;; Increase the height of the mode line
  (set-face-attribute 'mode-line nil
                      :box `(:line-width 2 :color ,(face-attribute 'mode-line :background)))
  (set-face-attribute 'mode-line-inactive nil
                      :box `(:line-width 2 :color ,(face-attribute 'mode-line-inactive :background)))

  ;; Fix tab bar faces
  (set-face-attribute 'tab-bar nil :foreground (face-attribute 'mode-line :foreground))
  (set-face-attribute 'tab-bar-tab nil :weight 'bold))

(advice-add 'enable-theme :after #'dw/activate-theme-tweaks)

;;; -- Mode Line -----

(setq-default mode-line-format
              '("%e" "  "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                mode-line-format-right-align
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                "  ")
              mode-line-percent-position nil
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

(use-package minions
  :init
  (minions-mode))

;;; -- Timers -----

(use-package tmr)

(defun dw/tmr-mode-line ()
  (if (not (and (boundp 'tmr--timers)
                tmr--timers))
      ""
    (propertize (format " 🕐 %s: %s"
                        (tmr--format-remaining (car tmr--timers))
                        (tmr--timer-description (car tmr--timers)))
                'tab-bar '(:foreground "orange"))))

;;; -- Beframe -----

(use-package beframe
  :ensure t
  :init
  (beframe-mode))

(defvar consult-buffer-sources)
(declare-function consult--buffer-state "consult")

(with-eval-after-load 'consult
  (defface beframe-buffer
    '((t :inherit font-lock-string-face))
    "Face for `consult' framed buffers.")

  (defun dw/beframe-buffer-names-sorted (&optional frame)
    "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
    (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

  ;; Hide full buffer list by default (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)

  (defvar beframe-consult-source
    `( :name     "Frame-specific buffers (current frame)"
       :narrow   ?F
       :category buffer
       :history  beframe-history
       :items    ,#'dw/beframe-buffer-names-sorted
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'beframe-consult-source))

;;   (setq consult-ripgrep-args "rg --null --hidden --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number --search-zip .")

;; -- Tab Bar -----

(defun dw/switch-tab-buffer (&optional arg)
  (interactive "P")
  (cond
   ((and arg (> (car arg) 0)) (call-interactively #'consult-buffer))
   ((project-current) (call-interactively #'project-switch-to-buffer))
   (t (call-interactively #'consult-buffer))))

(use-package tab-bar
  :ensure nil
  :bind (("s-[" . tab-bar-switch-to-prev-tab)
         ("s-]" . tab-bar-switch-to-next-tab)
         ("s-{" . (lambda ()
                    (interactive)
                    (tab-move -1)))
         ("s-}" . (lambda ()
                    (interactive)
                    (tab-move 1))))
  :custom
  (tab-bar-show t)
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width nil)
  (tab-bar-format '(tab-bar-format-menu-bar
                    dw/exwm-workspace-icon
                    tab-bar-format-tabs-groups
                    tab-bar-separator
                    dw/tmr-mode-line
                    tab-bar-separator
                    tab-bar-format-align-right
                    tab-bar-format-global))

  ;; Like winner-mode for tabs
  (tab-bar-history-mode 1)
  (tab-bar-mode 1))

;;; -- Notifications -----

(use-package alert
  :custom
  (alert-default-style (if dw/is-termux 'termux 'notifications)))

;;; -- Editing Configuration -----

(setq-default tab-width 2
              indent-tabs-mode nil)

(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode))

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  ;; (add-to-list 'super-save-predicates (lambda ()
  ;;                                       (not (eq major-mode 'mu4e-compose-mode))))
  )

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

(use-package paren
  :ensure nil
  :config
  (set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
  (show-paren-mode 1))

(use-package visual-fill-column
  :hook (org-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-width 110)
  (visual-fill-column-center-text t))

(use-package avy
  :bind (("C-'" . avy-goto-char)
         ("C-;" . avy-goto-char-timer))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-single-candidate-jump nil)
  :config
  (defun dw/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (setf (alist-get ?. avy-dispatch-alist) 'dw/avy-action-embark))

;;; -- Window Management -----

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-minibuffer-flag t))

(use-package winner
  :config
  (winner-mode))

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

(use-package popper
  :bind (("C-M-'" . popper-toggle-latest)
         ("M-'" . popper-cycle)
         ("C-M-\"" . popper-toggle-type))
  :custom
  (popper-window-height 12)
  (popper-reference-buffers '(eshell-mode
                              vterm-mode
                              geiser-repl-mode
                              help-mode
                              grep-mode
                              helpful-mode
                              compilation-mode))
  :config
  (require 'popper) ;; Needed because I disabled autoloads
  (popper-mode 1))

;;; -- Dired -----

(use-package all-the-icons-dired)
(use-package dired-ranger)

(defun dw/dired-mode-hook ()
  (interactive)
  (dired-hide-details-mode 1)
  (unless (or dw/is-termux
              (string-equal "/gnu/store/" (expand-file-name default-directory)))
    (all-the-icons-dired-mode 1))
  (hl-line-mode 1))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("b" . dired-up-directory)
              ("H" . dired-hide-details-mode))
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-dwim-target 'dired-dwim-target-next
        dired-hide-details-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t)

  (add-hook 'dired-mode-hook #'dw/dired-mode-hook)

  (unless dw/exwm-enabled
    (global-set-key (kbd "s-e") #'dired-jump)))

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

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;;; -- Make Help More Helpful -----

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind (([remap describe-function] . helpful-function)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key)))

;; Load the info system for info files
(add-to-list 'auto-mode-alist '("\\.info\\'" . Info-on-current-buffer))

;;; -- Foot Support -----

(add-to-list 'term-file-aliases '("foot" . "xterm"))

;;; -- Start the Daemon -----

(server-start)

(provide 'dw-core)
