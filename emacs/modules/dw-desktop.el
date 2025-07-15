;; -*- lexical-binding: t; -*-

;; Integrate with the system clipboard
(unless (display-graphic-p)
  (use-package xclip
    :demand t
    :config
    (xclip-mode 1)))

;; Pull in $PATH from the shell environment
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package bluetooth)
;;  :commands (bluetooth-list-devices))

;; Control NetworkManager via nmcli
(use-package nm
  :vc (:url "https://github.com/Kodkollektivet/emacs-nm"
       :rev :newest))

(use-package inf-mongo
  :vc (:url "https://github.com/endofunky/inf-mongo"
       :rev :newest))

(provide 'dw-desktop)
