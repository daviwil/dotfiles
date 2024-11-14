;; -*- lexical-binding: t; -*-

(defun dw/present-prepare-slide ()
  (when (and logos-focus-mode
             (derived-mode-p 'org-mode))
    (org-overview)
    (org-show-entry)
    (org-show-children)))

(defun dw/present-toggle ()
  "Configures the buffer for a presentation."
  (interactive)
  (if logos-focus-mode
      (progn
        (setq-local face-remapping-alist nil)
        (widen)
        (logos-focus-mode 0))

    (setq-local face-remapping-alist '((default (:height 1.5) default)
                                       (org-document-title (:height 1.75) org-document-title)
                                       (org-block-begin-line (:height 0.7) org-block)))

    ;; Narrow the buffer and start focus mode
    (logos-narrow-dwim)
    (logos-focus-mode 1)

    ;; Prepare the slide
    (dw/present-prepare-slide)))

(use-package logos
  :bind (([remap narrow-to-region] . logos-narrow-dwim)
	       ([remap forward-page] . logos-forward-page-dwim)
         ([remap backward-page] . logos-backward-page-dwim))
  :custom
  (logos-outlines-are-pages t)
  (logos-scroll-lock t)
  :config
  (setf (alist-get 'org-mode logos-outline-regexp-alist) "^\\*\\{1,2\\} +")
  (add-hook 'logos-page-motion-hook #'dw/present-prepare-slide))

(provide 'dw-present)
