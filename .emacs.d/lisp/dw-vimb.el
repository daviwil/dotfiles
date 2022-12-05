;; -*- lexical-binding: t; -*-

(setq dw/open-url-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") 'ivy-immediate-done)
    map))

(defun dw/open-url ()
  (interactive)
  (let ((history-items
          (with-temp-buffer
            (insert-file-contents "~/.config/vimb/history")
            (split-string (buffer-string) "\n" t))))
    (ivy-read "Open URL: " (remove-duplicates history-items :test #'string-equal)
              :keymap dw/open-url-map
              :action (lambda (item)
                        (start-process "vimb" nil "vimb" (car (split-string item (string ?\t))))))))
