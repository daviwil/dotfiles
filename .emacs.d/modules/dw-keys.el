;; -*- lexical-binding: t; -*-

;; -- Key Binding Helpers -----

(defun define-key* (keymap &rest keys)
  "Define multiple keys in a keymap."
  (while keys
    (define-key keymap
                (kbd (pop keys))
                (pop keys))))

(put 'define-key* 'lisp-indent-function 1)

;; -- Files Keymap -----

(defvar dw/files-prefix-map (make-sparse-keymap)
  "Keymap for common file operations.")

(global-set-key (kbd "C-c f") dw/files-prefix-map)

;; -- Git Keymap -----

(defvar dw/git-prefix-map (make-sparse-keymap)
  "Keymap for Git operations.")

(global-set-key (kbd "C-c g") dw/git-prefix-map)

;; -- Guix Keymap -----

(defvar dw/guix-prefix-map (make-sparse-keymap)
  "Keymap for Guix operations.")

(global-set-key (kbd "C-c G") dw/guix-prefix-map)

(provide 'dw-keys)
