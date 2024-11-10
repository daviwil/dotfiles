;; -*- lexical-binding: t; -*-

(defun dw/0x0-upload-text ()
  (interactive)
  (let* ((contents (if (use-region-p)
		       (buffer-substring-no-properties (region-beginning) (region-end))
		     (buffer-string)))
	 (temp-file (make-temp-file "0x0" nil ".txt" contents)))
    (message "Sending %s to 0x0.st..." temp-file)
    (let ((url (string-trim-right
		(shell-command-to-string
		 (format "curl -s -F'file=@%s' https://0x0.st" temp-file)))))
      (message "The URL is %s" url)
      (kill-new url)
      (delete-file temp-file))))

(defun dw/0x0-upload-file (file-path)
  (interactive "fSelect a file to upload: ")
  (message "Sending %s to 0x0.st..." file-path)
  (let ((url (string-trim-right
	      (shell-command-to-string
	       (format "curl -s -F'file=@%s' https://0x0.st" (expand-file-name file-path))))))
    (message "The URL is %s" url)
    (kill-new url)))

(provide 'dw-0x0)
