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

;; -- Org Keymap -----

(defvar dw/org-prefix-map (make-sparse-keymap)
  "Keymap for Org operations.")

(global-set-key (kbd "C-c o") dw/org-prefix-map)

;; -- Mail Keymap -----

(defvar dw/mail-prefix-map (make-sparse-keymap)
  "Keymap for mail operations.")

(global-set-key (kbd "C-c m") dw/mail-prefix-map)

;; -- Password Keymap -----

(defvar dw/password-prefix-map (make-sparse-keymap)
  "Keymap for password operations.")

(global-set-key (kbd "C-c p") dw/password-prefix-map)

;; -- Git Keymap -----

(defvar dw/git-prefix-map (make-sparse-keymap)
  "Keymap for Git operations.")

(global-set-key (kbd "C-c g") dw/git-prefix-map)

;; -- Guix Keymap -----

(defvar dw/guix-prefix-map (make-sparse-keymap)
  "Keymap for Guix operations.")

(global-set-key (kbd "C-c G") dw/guix-prefix-map)

(provide 'dw-keys)
