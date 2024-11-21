;;; dw-babel-ex.el --- Execute Babel blocks and progressively apply contents. -*- lexical-binding: t -*-

(require 'ob-core)
(require 'ob-tangle)

;;; Code:

(defun dw/org-babel--calculate-point (line)
  "Calculate a point for LINE in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line line))
    (point))

(defun dw/org-babel--replace-content (file-path content start-line end-line)
  "Replace a substring of text in a given file between two points."
  (with-current-buffer (find-file-noselect file-path)
    ;; Clear the buffer first if needed
    (let ((start (if start-line
                     (dw/org-babel--calculate-point (- start-line 1))
                   (point-min)))
          (end (if end-line
                   (dw/org-babel--calculate-point end-line)
                 (point-max))))
      (save-excursion
        (goto-char (point-min))
        (delete-region start end)
        (insert content)
        (save-buffer))))

  (message "Updated file %s." file-path))

(defun dw/org-babel-apply-blocks ()
  "Progressively executes blocks in the current buffer."
  (interactive)
  (org-babel-map-src-blocks nil
    (let* ((args (org-babel-parse-header-arguments header-args))
           (block (org-babel-tangle-single-block 0 t))
           (block-info (cadar block))
           (args (nth 5 block-info)) ;; TODO: Is there a function for this?
           (content (nth 6 block-info))
           (ex-value (alist-get :ex args))
           (start-line (alist-get :start-line args))
           (end-line (alist-get :end-line args))
           (base-dir (alist-get :dir args))
           (org-confirm-babel-evaluate nil))
      (when ex-value
        (if (string= lang "sh")
            (org-babel-execute-src-block)
          (dw/org-babel--replace-content
           (if base-dir
               (expand-file-name ex-value base-dir)
             ex-value)
           content
           start-line
           end-line))))))

(provide 'dw-babel-ex)
;;; dw-babel-ex.el ends here
