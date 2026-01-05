;; -*- lexical-binding: t; -*-

(use-package denote
  :demand t
  :bind (("C-c n l" . denote-link-or-create)
         ("C-c n o" . denote-open-or-create)
         ("C-c n r" . denote-rename-file-using-front-matter))
  :custom
  (denote-directory "~/Notes/Log")
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
  ;; Rename buffers with the note name
  (denote-rename-buffer-mode 1)

  ;; Buttonize all denote links in text buffers
  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe))

(use-package consult-notes
  :ensure nil
  :commands (consult-notes)
  :config
  (consult-notes-denote-mode 1))

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

(defun dw/orgalist-text-mode-hook ()
  (unless (derived-mode-p 'org-mode)
    (orgalist-mode 1)))

(use-package orgalist
  :init
  (add-hook 'text-mode-hook 'dw/orgalist-text-mode-hook))

(defun dw/howm-set-buffer-name ()
  (when (and buffer-file-name
             (howm-subdirectory-p howm-directory buffer-file-name))
    (howm-mode-set-buffer-name)))

(use-package howm
  :ensure nil
  :bind* ("C-c ; ;" . howm-menu)
  :init
  (setq howm-prefix (kbd "C-c ;")
        howm-view-use-grep t
        howm-buffer-name-format "howm: %s"
        howm-buffer-name-limit 100
        howm-buffer-name-total-limit 100
        howm-directory "~/Notes"
        howm-keyword-file (expand-file-name ".howm-keys" howm-directory)
        howm-history-file (expand-file-name ".howm-history" howm-directory)
        howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org"
        howm-view-title-header "*"
        howm-dtime-format "<%Y-%m-%d %a %H:%M>"
        howm-template "* %title%cursor\n\n%date %file\n\n")
  :config
  (add-hook 'org-mode-hook 'howm-mode)
  (add-hook 'howm-mode-hook 'dw/howm-set-buffer-name)
  (add-hook 'after-save-hook 'dw/howm-set-buffer-name)

  ;; Adapted from this GitHub issue:
  ;; https://github.com/protesilaos/modus-themes/issues/117#issuecomment-2337993946
  (modus-themes-with-colors
    (custom-set-faces
     `(howm-menu-key-face ((,c :inherit help-key-binding)))
     `(howm-mode-keyword-face (( )))
     `(howm-mode-ref-face ((,c :inherit link)))
     `(howm-mode-title-face ((,c :inherit modus-themes-heading-0)))
     `(howm-mode-wiki-face ((,c :inherit link)))
     `(howm-reminder-deadline-face ((,c :foreground ,red-warmer :underline nil)))
     `(howm-reminder-late-deadline-face ((,c :inherit bold :foreground ,date-deadline :underline nil)))
     `(howm-reminder-defer-face ((,c :foreground ,date-scheduled :underline nil)))
     `(howm-reminder-schedule-face ((,c :foreground ,date-scheduled :underline nil)))
     `(howm-reminder-done-face ((,c :foreground ,prose-done :underline nil)))
     `(howm-reminder-todo-face ((,c :foreground ,prose-todo :underline nil)))
     `(howm-reminder-normal-face ((,c :foreground ,date-common :underline nil)))
     `(howm-reminder-today-face ((,c :inherit bold :foreground ,bg-main :background ,yellow-warmer :underline nil)))
     `(howm-reminder-tomorrow-face ((,c :inherit bold :foreground ,date-scheduled :underline nil)))
     `(howm-simulate-todo-mode-line-face ((,c :inherit bold)))
     `(howm-view-empty-face (( )))
     `(howm-view-hilit-face ((,c :inherit match)))
     `(howm-view-name-face ((,c :inherit bold)))
     `(iigrep-counts-face1 ((,c :foreground ,rainbow-1)))
     `(iigrep-counts-face2 ((,c :foreground ,rainbow-2)))
     `(iigrep-counts-face3 ((,c :foreground ,rainbow-3)))
     `(iigrep-counts-face4 ((,c :foreground ,rainbow-4)))
     `(iigrep-counts-face5 ((,c :foreground ,rainbow-5))))))

(provide 'dw-writing)
