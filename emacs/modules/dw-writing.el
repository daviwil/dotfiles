;; -*- lexical-binding: t; -*-

(use-package denote
  :demand t
  :bind (("C-c n l" . denote-link-or-create)
         ("C-c n o" . denote-open-or-create)
         ("C-c n r" . denote-rename-file-using-front-matter))
  :custom
  (denote-directory "~/Notes/Denote")
  (denote-rename-buffer-format "Denote: %t (%k)")
  (denote-infer-keywords nil)
  (denote-known-keywords
   '("pra" "prb" "prc"
     "ply" "plm" "plw"
     "kt" "ke" "kp" "kl" "ka" "kap"
     "kcp" "kca" "kcc"
     "kra" "krb" "krv"
     "rn"))

  :config

  (require 'denote-rename-buffer)
  (require 'denote-org-extras)

  ;; Rename buffers with the note name
  (denote-rename-buffer-mode 1)

  ;; Buttonize all denote links in text buffers
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer))

(defun dw/setup-markdown-mode ()
  (center-document-mode 1)
  (display-line-numbers-mode 0))

(use-package markdown-mode
  :config
  (setq markdown-command "marked")
  (add-hook 'markdown-mode-hook #'dw/setup-markdown-mode)
  (dolist (face '((markdown-header-face-1 . 1.2)
                  (markdown-header-face-2 . 1.1)
                  (markdown-header-face-3 . 1.0)
                  (markdown-header-face-4 . 1.0)
                  (markdown-header-face-5 . 1.0)))
    (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

(provide 'dw-writing)
