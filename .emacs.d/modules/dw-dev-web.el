;; -*- lexical-binding: t; -*-

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (setq-default js-indent-level 2))

(defun dw/setup-markdown-mode ()
  (visual-fill-column-mode 1)
  (display-line-numbers-mode 0))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (add-hook 'markdown-mode-hook #'dw/setup-markdown-mode)
  (dolist (face '((markdown-header-face-1 . 1.2)
                  (markdown-header-face-2 . 1.1)
                  (markdown-header-face-3 . 1.0)
                  (markdown-header-face-4 . 1.0)
                  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :ensure t)
(use-package skewer-mode)

(with-eval-after-load 'simple-httpd
  (add-to-list 'httpd-mime-types '("wasm" . "application/wasm")))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(provide 'dw-dev-web)
