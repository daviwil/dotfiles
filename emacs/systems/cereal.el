(setq dw/use-config-modules
 (append dw/common-config-modules
         '(dw-desktop
           dw-system
           dw-develop
           dw-ai)))

;; Treat Command as Ctrl inside of Emacs (because I remap Ctrl to Command)
(setq ns-command-modifier 'meta)

;; Use the traditional approach to setting alpha on macOS
(set-frame-parameter (selected-frame) 'alpha 93)
(add-to-list 'default-frame-alist '(alpha . 93))

;; Use ls from coreutils in Dired
(setq insert-directory-program "gls"
      dired-use-ls-dired t)

;; Adjust font sizes
;; TODO: Use defvars instead
(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'default nil :height 166)
            (set-face-attribute 'fixed-pitch nil :height 166)
            (set-face-attribute 'variable-pitch nil :height 146)))
