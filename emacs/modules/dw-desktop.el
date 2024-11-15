;; -*- lexical-binding: t; -*-

;; Integrate with the system clipboard
(unless (display-graphic-p)
  (use-package xclip
    :demand t
    :config
    (xclip-mode 1)))

(use-package bluetooth)

;; Control NetworkManager via nmcli
(use-package nm
  :vc (:url "https://github.com/Kodkollektivet/emacs-nm"
       :rev :newest))

(provide 'dw-desktop)
