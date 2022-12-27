;; -*- lexical-binding: t; -*-

(setup (:pkg hydra)
  (require 'hydra))

(setup (:pkg vertico)
  (vertico-mode)
  (:with-map vertico-map
    (:bind "C-j" vertico-next
           "C-k" vertico-previous
           "C-f" vertico-exit-input))
  (:with-map minibuffer-local-map
    (:bind "M-h" vertico-directory-up))
  (:option vertico-cycle t)
  (custom-set-faces '(vertico-current ((t (:background "#3a3f5a"))))))

(setup (:pkg corfu)
  (:with-map corfu-map
    (:bind "C-j" corfu-next
           "C-k" corfu-previous
           "TAB" corfu-insert
           [tab] corfu-insert
           "C-f" corfu-insert))
  (:option corfu-cycle t
           corfu-auto t
           corfu-preview-current nil
           corfu-quit-at-boundary t
           corfu-quit-no-match t)
  (global-corfu-mode 1))

(setup (:pkg kind-icon)
  (:load-after corfu)
  (:option kind-icon-default-face 'corfu-default)
  (:when-loaded
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(setup (:pkg orderless)
  (require 'orderless)

  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        orderless-matching-styles '(orderless-literal orderless-regexp)
        completion-category-overrides
         '((file (styles partial-completion)))))
           ;(command (styles orderless+initialism))
           ;(symbol (styles orderless-flex orderless-literal))
           ;(variable (styles orderless+initialism)))))

(setup (:pkg wgrep)
  (add-hook 'grep-mode-hook #'wgrep-setup))

(setup (:pkg consult)
  (require 'consult)
  (:also-load wgrep)
  (:global "C-s" consult-line
           "C-M-l" consult-imenu)

  (:with-map minibuffer-local-map
    (:bind "C-r" consult-history))

  (defun dw/get-project-root ()
    (when (fboundp 'projectile-project-root)
      (projectile-project-root)))

  (:option consult-project-root-function #'dw/get-project-root
           completion-in-region-function #'consult-completion-in-region))

(setup (:pkg consult-dir)
  (:global "C-x C-d" consult-dir)
  (:with-map vertico-map
    (:bind "C-x C-d" consult-dir
           "C-x C-j" consult-dir-jump-file))
  (:option consult-dir-project-list-function nil))

(setup (:pkg marginalia)
  (:option marginalia-annotators '(marginalia-annotators-heavy
                                   marginalia-annotators-light
                                   nil))
  (marginalia-mode))

(setup (:pkg embark)
  (:also-load embark-consult)
  (:global "C-M-." embark-act)
  (:with-map minibuffer-local-map
   (:bind "C-d" embark-act))

  ;; Use Embark to show command prefix help
  (setq prefix-help-command #'embark-prefix-help-command))

(provide 'dw-interface)
