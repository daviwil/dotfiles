(in-package :next)

(defvar *dw-keymap* (make-keymap)
  "Keymap for `dw-mode'.")

(define-mode dw-mode ()
  "Custom mode for the custom key bindings in `*dw-keymap*'."
  ((keymap-schemes :initform (list :emacs-map *dw-keymap*
                                   :vi-normal *dw-keymap*))))

;; Gimmie that Qutebrowser feel
(define-key :keymap *dw-keymap* "C-d" #'scroll-page-down)
(define-key :keymap *dw-keymap* "C-u" #'scroll-page-up)
(define-key :keymap *dw-keymap* "J" #'switch-buffer-previous)
(define-key :keymap *dw-keymap* "K" #'switch-buffer-next)

(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)
(add-to-default-list 'dw-mode 'buffer 'default-modes)

(defun my-buffer-defaults (buffer)
  (setf (zoom-ratio-default buffer) 1.6)
  (unzoom-page :buffer buffer))          ; Needed for non-web-mode buffers.

(defun my-interface-defaults ()
  (hooks:add-to-hook (hooks:object-hook *interface* 'buffer-make-hook)
                     #'my-buffer-defaults))

(hooks:add-to-hook '*after-init-hook* #'my-interface-defaults)
