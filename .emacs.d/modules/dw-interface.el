;; -*- lexical-binding: t; -*-

(use-package hydra)

(use-package icomplete
  :disabled
  :ensure nil
  :custom
  (icomplete-scroll t)
  (icomplete-show-matches-on-no-input t)
  (icomplete-vertical-prospects-height 1)
  (icomplete-compute-delay 0.05)

  :init
  (icomplete-vertical-mode 1))

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit-input)
              :map minibuffer-local-map
              ("M-h" . vertico-directory-up))
  :custom
  (vertico-cycle t)

  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))

  :config
  (require 'vertico-directory)
  (vertico-mode))

(use-package corfu
  :bind (:map corfu-map
              ("C-j" . corfu-next)
              ("C-k" . corfu-previous)
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert)
              ("C-f" . corfu-insert))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)

  :config
  (global-corfu-mode 1)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(use-package kind-icon
  :after corfu
  :custom (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :demand t
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        orderless-matching-styles '(orderless-literal orderless-regexp)
        completion-category-overrides
        '((file (styles partial-completion)))))

(use-package wgrep
  :after consult
  :hook (grep-mode . wgrep-setup))

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         :map minibuffer-local-map
         ("C-r" . consult-history))

  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region)

  :config
  (defun dw/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root))))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))

  :custom
  (consult-dir-project-list-function nil))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :config
  (marginalia-mode))

(use-package embark
  :after vertico
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         :map minibuffer-local-map
         ("C-d" . embark-act)
         :map embark-region-map
         ("D" . denote-region))

  :config
  ;; Remove the mixed indicator to prevent the popup from being displayed
  ;; automatically
  (delete #'embark-mixed-indicator embark-indicators)
  (add-to-list 'embark-indicators 'embark-minimal-indicator)

  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after embark)

(provide 'dw-interface)
