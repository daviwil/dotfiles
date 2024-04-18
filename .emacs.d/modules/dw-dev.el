;; -*- lexical-binding: t; -*-

;;; -- Paren Matching -----

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-use-smartparens-bindings))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (org-mode
         emacs-lisp-mode
         web-mode
         typescript-mode
         js2-mode))

;;; -- Buffer Environments -----

(use-package buffer-env
  :custom
  (buffer-env-script-name "manifest.scm"))

;;; -- M-x compile -----

(use-package compile
  :custom
  (compilation-scroll-output t))

(setq compilation-environment '("TERM=xterm-256color"))

(defun dw/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'dw/advice-compilation-filter)

(defun dw/auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

;;; -- project.el -----

(defun dw/current-project-name ()
  (file-name-nondirectory
   (directory-file-name
    (project-root (project-current)))))

(defun dw/switch-project-action ()
  (interactive)
  (let* ((project-name (dw/current-project-name))
         (tab-bar-new-tab-choice #'magit-status)
         (tab-index (tab-bar--tab-index-by-name project-name)))
    (if tab-index
        (tab-bar-select-tab (1+ tab-index))
      (tab-bar-new-tab)
      (tab-bar-rename-tab project-name))))

(defun dw/close-project-tab ()
  (interactive)
  (let* ((project-name (dw/current-project-name))
         (tab-index (tab-bar--tab-index-by-name project-name)))
    (project-kill-buffers t)
    (when tab-index
      (tab-bar-close-tab (1+ tab-index)))))

(defun dw/project-magit-status ()
  (interactive)
  (magit-status (project-root (project-current))))

(use-package project
  :bind (("C-M-p" . project-find-file)
         :map project-prefix-map
         ("k" . dw/close-project-tab)
         ("F" . consult-ripgrep))

  :config
  (add-to-list 'project-switch-commands '(dw/project-magit-status "Magit" "m"))
  (add-to-list 'project-switch-commands '(consult-ripgrep "Ripgrep" "F")))

;;; -- Eglot -----

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c C-a" . eglot-code-actions)
              ("C-c C-r" . eglot-rename))
  :hook (((c-mode c++-mode) . eglot-ensure)
         ((js2-mode typescript-mode) . eglot-ensure)
         (rust-mode . eglot-ensure))

  :config
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil)
  ;; TODO: Is this needed now?
  (add-to-list 'eglot-server-programs
               '((js2-mode typescript-mode) . ("typescript-language-server" "--stdio"))))

;;; -- Magit -----

(use-package magit
  :bind ("C-M-;" . magit-status-here)
  :custom
  (magit-show-long-lines-warning nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))

(defhydra dw/smerge-panel ()
  "smerge"
  ("k" (smerge-prev) "prev change" )
  ("j" (smerge-next) "next change")
  ("u" (smerge-keep-upper) "keep upper")
  ("l" (smerge-keep-lower) "keep lower")
  ("q" nil "quit" :exit t))

;;; -- Git -----

(use-package git-link
  :config
  (setq git-link-open-in-browser t)
  (define-key dw/git-prefix-map "l" 'git-link))

;; (setup (:pkg git-gutter :straight git-gutter-fringe)
;;   (:hook-into text-mode prog-mode)
;;   (setq git-gutter:update-interval 2)
;;   (unless dw/is-termux
;;     (require 'git-gutter-fringe)
;;     (set-face-foreground 'git-gutter-fr:added "LightGreen")
;;     (fringe-helper-define 'git-gutter-fr:added nil
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       ".........."
;;       ".........."
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       ".........."
;;       ".........."
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX")

;;     (set-face-foreground 'git-gutter-fr:modified "LightGoldenrod")
;;     (fringe-helper-define 'git-gutter-fr:modified nil
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       ".........."
;;       ".........."
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       ".........."
;;       ".........."
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX")

;;     (set-face-foreground 'git-gutter-fr:deleted "LightCoral")
;;     (fringe-helper-define 'git-gutter-fr:deleted nil
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       ".........."
;;       ".........."
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       ".........."
;;       ".........."
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"
;;       "XXXXXXXXXX"))

;;   ;; These characters are used in terminal mode
;;   (setq git-gutter:modified-sign "≡")
;;   (setq git-gutter:added-sign "≡")
;;   (setq git-gutter:deleted-sign "≡")
;;   (set-face-foreground 'git-gutter:added "LightGreen")
;;   (set-face-foreground 'git-gutter:modified "LightGoldenrod")
;;   (set-face-foreground 'git-gutter:deleted "LightCoral"))

;;; -- Code Formatting -----

(use-package apheleia
  :hook (prog-mode . apheleia-mode))

(use-package lispy
  :disabled t
  :hook (emacs-lisp-mode scheme-mode))

(use-package lispyville
  :if (and (boundp 'evil) evil-mode)
  :disabled t
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme '(operators c-w additional
                                        additional-movement slurp/barf-cp
                                        prettify)))

;;; -- Text Files -----

(defun dw/display-column-indicator-in-text-mode ()
  (when (equal major-mode #'text-mode)
    (display-fill-column-indicator-mode)))

(use-package text-mode
  :ensure nil
  :hook (text-mode . dw/display-column-indicator-in-text-mode))

;;; -- Emacs Lisp -----

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

;;; -- Common Lisp -----

(use-package sly
  :disabled
  :mode "\\.lisp\\'")

;;; -- Scheme -----

;; Include .sld library definition files
(use-package scheme-mode
  :ensure nil
  :mode "\\.sld\\'")

(use-package geiser
  :config
  ;; (setq geiser-default-implementation 'gambit)
  ;; (setq geiser-active-implementations '(gambit guile))
  ;; (setq geiser-implementations-alist '(((regexp "\\.scm$") gambit)
  ;;                                      ((regexp "\\.sld") gambit)))
  ;; (setq geiser-repl-default-port 44555) ; For Gambit Scheme
  (setq geiser-default-implementation 'guile)
  (setq geiser-active-implementations '(guile))
  (setq geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(use-package geiser-guile
  :after geiser
  :config
  (when (eq dw/current-distro 'void)
    (setq geiser-guile-binary "guile3")))

;; (with-eval-after-load 'geiser-guile
;;   ;; (add-to-list 'geiser-guile-load-path "~/.dotfiles")
;;   (add-to-list 'geiser-guile-load-path "~/Projects/Code/guix"))

;; This is needed for contributing to Guix source
(setq user-mail-address "david@daviwil.com")
(add-hook 'after-save-hook 'copyright-update)
(setq copyright-names-regexp
      (format "%s <%s>" user-full-name user-mail-address))

;;; -- Mesche -----

(use-package mesche
  :ensure nil
  :load-path "~/Projects/Code/mesche/mesche-emacs"
  :mode "\\.msc\\'")

;;; -- Snippets -----

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;;; -- TypeSpec -----

(use-package adl-mode
  :ensure nil
  :mode "\\.\\(cadl\\|tsp\\)\\'"
  :hook (adl-mode . abbrev-mode)
  :bind ("C-c C-c" . recompile))

;;; -- Rust -----

(use-package rust-mode
  :disabled)

;;; -- Zig -----

(use-package zig-mode
  :disabled)

;;; -- Lua -----

(use-package lua-mode)

(provide 'dw-dev)
