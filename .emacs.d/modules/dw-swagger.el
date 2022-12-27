;; -*- lexical-binding: t; -*-

(require 'json)
(require 'subr-x)

(defun dw/swagger-get-actions (path)
  (string-join
   (mapcar (lambda (action)
             (upcase (format "%s" (car action))))
           path)
   ", "))

;;;###autoload
(defun dw/swagger-extract-paths ()
  "Extract URI paths and HTTP actions from current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((paths (mapcar (lambda (path)
                           (format "- %s [%s]"
                                   (car path)
                                   (dw/swagger-get-actions (cdr path))))
                         (alist-get 'paths (json-read)))))
      (with-current-buffer (get-buffer-create "*swagger-extract*")
        (goto-char (point-min))
        (delete-region (point-min) (point-max))
        (insert (string-join paths "\n"))
        (switch-to-buffer (current-buffer))))))

;; (progn
;;   (with-current-buffer "metricsadvisor.json"
;;     (dw/swagger-extract-paths)))

(provide 'dw-swagger)
