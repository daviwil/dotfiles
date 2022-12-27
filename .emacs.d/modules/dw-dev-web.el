;; -*- lexical-binding: t; -*-

(setup (:pkg typescript-mode)
  (:file-match "\\.ts\\'")
  (:hook eglot-ensure)
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq-default js-indent-level 2)
  (setq-default evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(setup (:pkg js2-mode)
  (:file-match "\\.jsx?\\'")

  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(setup (:pkg markdown-mode)
  (setq markdown-command "marked")
  (:file-match "\\.md\\'")
  (:when-loaded
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))))

(setup (:pkg web-mode)
  (:file-match "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'")
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(setup (:pkg impatient-mode :straight t))
(setup (:pkg skewer-mode))

(with-eval-after-load 'simple-httpd
  (add-to-list 'httpd-mime-types '("wasm" . "application/wasm")))

(setup (:pkg yaml-mode)
  (:file-match "\\.ya?ml\\'"))

(provide 'dw-dev-web)
