;; -*- lexical-binding: t; -*-

(use-package bluetooth)

;; Control NetworkManager via nmcli
(use-package nm
  :vc (:url "https://github.com/Kodkollektivet/emacs-nm"
       :rev :newest))

(provide 'dw-desktop)
