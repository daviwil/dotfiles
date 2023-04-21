;;; -- Set up package.el and use-package -----

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Automatically install packages when not in Guix
(setq use-package-always-ensure (not dw/is-guix-system))

;; Never load a package until demanded or triggered
;;(setq use-package-always-defer t)

(provide 'dw-package)
