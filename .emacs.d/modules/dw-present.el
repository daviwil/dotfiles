;; -*- lexical-binding: t; -*-

(defun dw/org-present-prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) default)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     ;; These were needed before the Wayland transition
                                     ;; (org-code (:height 1.55) org-code)
                                     ;; (org-verbatim (:height 1.55) org-verbatim)
                                     ;; (org-block (:height 1.1) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-display-inline-images)
  (dw/org-present-prepare-slide)
  (when (fboundp 'dw/kill-panel)
    (dw/kill-panel)))

(defun dw/org-present-quit-hook ()
  (setq-local face-remapping-alist nil)
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (org-appear-mode 1)
  (when (fboundp 'dw/start-panel)
    (dw/start-panel)))

(defun dw/org-present-prev ()
  (interactive)
  (org-present-prev)
  (dw/org-present-prepare-slide))

(defun dw/org-present-next ()
  (interactive)
  (org-present-next)
  (dw/org-present-prepare-slide)
  (when (fboundp 'live-crafter-add-timestamp)
    (live-crafter-add-timestamp (substring-no-properties (org-get-heading t t t t)))))

(use-package org-present
  :commands org-present
  :hook ((org-present-mode . dw/org-present-hook)
         (org-present-mode-quit . dw/org-present-quit-hook))
  :bind (:map org-present-mode-keymap
              ("C-c C-j" . dw/org-present-next)
              ("C-c C-k" . dw/org-present-prev)))

(provide 'dw-present)
