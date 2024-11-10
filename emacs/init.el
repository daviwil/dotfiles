;; -*- lexical-binding: t; -*-

;;; This file is generated from the Emacs.org file in my dotfiles repository!

;;; ----- Basic Configuration -----

;; Core settings
(setq ;; Flash the UI instead of beeping
      visible-bell t

      ;; Yes, this is Emacs
      inhibit-startup-message t

      ;; Instruct auto-save-mode to save to the current file, not a backup file
      auto-save-default nil

      ;; No backup files, please
      make-backup-files nil

      ;; Make it easy to cycle through previous items in the mark ring
      set-mark-command-repeat-pop t

      ;; Don't warn on large files
      large-file-warning-threshold nil

      ;; Follow symlinks to VC-controlled files without warning
      vc-follow-symlinks t

      ;; Don't warn on advice
      ad-redefinition-action 'accept

      ;; Revert Dired and other buffers
      global-auto-revert-non-file-buffers t

      ;; Silence compiler warnings as they can be pretty disruptive
      native-comp-async-report-warnings-errors nil)

;; Core modes
(repeat-mode 1)                ;; Enable repeating key maps
(menu-bar-mode 0)              ;; Hide the menu bar
(tool-bar-mode 0)              ;; Hide the tool bar
(savehist-mode 1)              ;; Save minibuffer history
(scroll-bar-mode 0)            ;; Hide the scroll bar
(xterm-mouse-mode 1)           ;; Enable mouse events in terminal Emacs
(display-time-mode 1)          ;; Display time in mode line / tab bar
(fido-vertical-mode 1)         ;; Improved vertical minibuffer completions
(column-number-mode 1)         ;; Show column number on mode line
(tab-bar-history-mode 1)       ;; Remember previous tab window configurations
(auto-save-visited-mode 1)     ;; Auto-save files at an interval
(global-visual-line-mode 1)    ;; Visually wrap long lines in all buffers
(global-auto-revert-mode 1)    ;; Refresh buffers with changed local files

;; Tabs to spaces
(setq-default indent-tabs-mode nil
	            tab-width 2)

;; Display line numbers in programming modes
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Make icomplete slightly more convenient
(keymap-set icomplete-fido-mode-map "M-h" 'icomplete-fido-backward-updir)
(keymap-set icomplete-fido-mode-map "TAB" 'icomplete-force-complete)

;; Delete trailing whitespace before saving buffers
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Automatically install packages but don't load them until requested
(setq use-package-always-ensure t
      use-package-always-defer t)

;; Move customization settings out of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file t))

;; Match completion substrings that may be out of order
(defun dw/override-fido-completion-styles ()
  (setq-local completion-styles '(substring partial-completion emacs22)))

(add-hook 'icomplete-minibuffer-setup-hook 'dw/override-fido-completion-styles)

;;; ----- Configuration Management -----

(defvar dw/use-config-modules '()
  "A list of module symbols to load once init.el is finished.")

;; Add configuration modules to load path
(add-to-list 'load-path '"~/.dotfiles/emacs/modules")

;; Load system-specific configuration
(let ((config-path
       (format "~/.dotfiles/emacs/systems/%s.el" system-name)))
  (if (file-exists-p config-path)
      (load-file config-path)
    (message "No per-system configuration found for %s!" system-name)))

;;; ----- Appearance -----

(defun dw/clear-background-color (&optional frame)
  (or frame (setq frame (selected-frame)))
  "unsets the background color in terminal mode"
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

;; Clear the background color for transparent terminals
(unless (display-graphic-p)
  (add-hook 'after-make-frame-functions 'dw/clear-background-color)
  (add-hook 'window-setup-hook 'dw/clear-background-color)
  (add-hook 'ef-themes-post-load-hook 'dw/clear-background-color))

;; Set preferred themes
(use-package ef-themes
  :demand t
  :custom (ef-themes-to-toggle '(ef-dream ef-owl))
  :config
  (ef-themes-select 'ef-dream))

;; Make vertical window separators look nicer in terminal Emacs
(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

;; Clean up the mode line
(setq-default mode-line-format
              '("%e" "  "
                (:propertize
                 ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote))
                mode-line-frame-identification
                mode-line-buffer-identification
                "   "
                mode-line-position
                mode-line-format-right-align
                "  "
                (project-mode-line project-mode-line-format)
                " "
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                mode-line-misc-info
                "  ")
              project-mode-line t
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '(" %l:%c"))

;; Move global mode string to the tab-bar and hide tab close buttons
(setq tab-bar-close-button-show nil
      tab-bar-format '(tab-bar-format-menu-bar
                       tab-bar-format-tabs-groups
                       tab-bar-separator
                       tab-bar-format-align-right
                       tab-bar-format-global))

;; Turn on the tab-bar
(tab-bar-mode 1)

;; Customize time display
(setq display-time-load-average nil
      display-time-format "%l:%M %p %b %d W%U"
      display-time-world-time-format "%a, %d %b %I:%M %p %Z"
      display-time-world-list
      '(("Etc/UTC" "UTC")
        ("Europe/Athens" "Athens")
        ("America/Los_Angeles" "Seattle")
        ("America/Denver" "Denver")
        ("America/New_York" "New York")
        ("Pacific/Auckland" "Auckland")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Kolkata" "Hyderabad")))

;; ----- Special Buffers as Popup Window -----

(setq dw/display-buffer-popup-rule
      '("\\*\\(shell\\|.*term\\|.*eshell\\|help\\|compilation\\).*\\*"
        (display-buffer-reuse-window display-buffer-in-side-window)
        (side . bottom)		         ; Popups go at the bottom
        (slot . 0)		         ; Use the first slot at the bottom
	(post-command-select-window . t) ; Select the window upon display
        (window-height . 0.3)))	         ; 30% of the frame height

(setq display-buffer-alist (list dw/display-buffer-popup-rule))

(defun dw/toggle-popup-window ()
  (interactive)
  (if-let ((popup-window
	    (get-window-with-predicate
	     (lambda (window)
	       (eq (window-parameter window 'window-side)
		   'bottom)))))

      ;; Focus the window if it is not selected, otherwise close it
      (if (eq popup-window (selected-window))
	  (delete-window popup-window)
	(select-window popup-window))

    ;; Find the most recent buffer that matches the rule and show it
    (if-let ((popup-buffer
	      (seq-find (lambda (buffer)
			  (buffer-match-p (car dw/display-buffer-popup-rule)
					  (buffer-name buffer)))
			(if (project-current)
			    (project-buffers (project-current))
			  (buffer-list (selected-frame))))))
	(display-buffer popup-buffer (cdr dw/display-buffer-popup-rule))
      (message "No popup buffers found."))))

;; TODO: This binding may need to change
(keymap-global-set "C-c p" #'dw/toggle-popup-window)

;;; ----- Essential Org Mode Configuration -----

;; Indent org-mode buffers for readability
(add-hook 'org-mode-hook #'org-indent-mode)

;; Use org-tempo
(use-package org-tempo
  :ensure nil
  :demand t
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("yaml" . "src yaml")
                  ("json" . "src json")))
    (add-to-list 'org-structure-template-alist item)))

;;; ----- Document Centering -----

(defvar center-document-desired-width 90
  "The desired width of a document centered in the window.")

(defun center-document--adjust-margins ()
  ;; Reset margins first before recalculating
  (set-window-parameter nil 'min-margins nil)
  (set-window-margins nil nil)

  ;; Adjust margins if the mode is on
  (when center-document-mode
    (let ((margin-width (max 0
			     (truncate
			      (/ (- (window-width)
				    center-document-desired-width)
				 2.0)))))
      (when (> margin-width 0)
	(set-window-parameter nil 'min-margins '(0 . 0))
	(set-window-margins nil margin-width margin-width)))))

(define-minor-mode center-document-mode
  "Toggle centered text layout in the current buffer."
  :lighter " Centered"
  :group 'editing
  (if center-document-mode
      (add-hook 'window-configuration-change-hook #'center-document--adjust-margins 'append 'local)
    (remove-hook 'window-configuration-change-hook #'center-document--adjust-margins 'local))
  (center-document--adjust-margins))

(add-hook 'org-mode-hook #'center-document-mode)

(defun dw/dired-mode-hook ()
  (interactive)
  (dired-hide-details-mode 1)
  (hl-line-mode 1))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("b" . dired-up-directory))
  :config
  (setq dired-listing-switches "-alv --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-dwim-target 'dired-dwim-target-next
        dired-hide-details-hide-symlink-targets nil
        dired-kill-when-opening-new-dired-buffer t
        delete-by-moving-to-trash t)

  (add-hook 'dired-mode-hook #'dw/dired-mode-hook))

;; Use `pass` as an auth-source
;;(auth-source-pass-enable)

;; Enable GPG passphrase entry
(use-package pinentry)

;; Load requested configuration modules
(dolist (module dw/use-config-modules)
  (require module))
