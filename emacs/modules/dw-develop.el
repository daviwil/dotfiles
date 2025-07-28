(use-package project
  :ensure nil
  :custom
  (project-switch-use-entire-map t))

(defun dw/project-compilation-buffer-name-function (name-of-mode)
  (format "*compilation: %s*" (project-name (project-current))))

(setq project-compilation-buffer-name-function 'dw/project-compilation-buffer-name-function)

(use-package eglot
  :ensure nil
  :hook ((js-mode
          typescript-mode) . eglot-ensure))

(use-package js-mode
  :ensure nil
  :config
  (setq-default js-indent-level 2))

(use-package typescript-mode
  :mode ("\\.tsx?\\'")
  :config
  (setq-default typescript-indent-level 2))

(use-package janet-mode)

(provide 'dw-develop)
