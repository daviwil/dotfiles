(in-package :next)

(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)

(defun my-buffer-defaults (buffer)
  (setf (zoom-ratio-default buffer) 1.6)
  (unzoom-page :buffer buffer))          ; Needed for non-web-mode buffers.

(defun my-interface-defaults ()
  (hooks:add-to-hook (hooks:object-hook *interface* 'buffer-make-hook)
                     #'my-buffer-defaults))

(hooks:add-to-hook '*after-init-hook* #'my-interface-defaults)
