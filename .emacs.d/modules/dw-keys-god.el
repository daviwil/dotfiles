;; -*- lexical-binding: t; -*-

(defun dw/god-quit ()
  "Go back to god-local-mode and quit any active minibuffer."
  (interactive)
  (god-local-mode 1)
  (keyboard-escape-quit))

(use-package god-mode
  :demand t
  :bind (("<escape>" . dw/god-quit)
          ("C-x C-1" . delete-other-windows)
          ("C-x C-2" . split-window-below)
          ("C-x C-3" . split-window-right)
          ("C-x C-0" . delete-window)
          ("C-x C-r" . eval-region)
          :map god-local-mode-map
          ("i" . god-local-mode)
          ("." . repeat))

  :config
  (god-mode)

  (defun dw/god-mode-update-cursor ()
    "Change cursor type based on god-local-mode."
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar)))

  (add-hook 'post-command-hook #'dw/god-mode-update-cursor)

  ;; Other useful keys:
  ;; x j - dired-jump

  (with-eval-after-load 'lispy
    (add-to-list 'lispy-compat 'god-mode)))

(provide 'dw-keys-god)
