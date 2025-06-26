(use-package eglot
  :ensure nil
  :hook ((js-mode
          typescript-mode) . eglot-ensure))

(use-package js-mode
  :ensure nil
  :config
  (setq-default js-indent-level 2))

(use-package typescript-mode
  :ensure nil
  :config
  (setq-default typescript-indent-level 2))
