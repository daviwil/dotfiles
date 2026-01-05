(use-package project
  :ensure nil
  :bind (:map project-prefix-map
              ("t" . eat-project))
  :custom
  (project-switch-use-entire-map t))

(defun dw/project-compilation-buffer-name-function (name-of-mode)
  (format "*compilation: %s*" (project-name (project-current))))

(setq project-compilation-buffer-name-function 'dw/project-compilation-buffer-name-function)

(defun dw/ensure-project-in-tab (project-dir)
  "Ensure we're in a tab named after the project."
  (let* ((project-name (file-name-nondirectory (directory-file-name project-dir)))
         (tab-names (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
         (existing-tab (seq-find (lambda (name) (equal name project-name)) tab-names)))
    (unless existing-tab
      (tab-new)
      (tab-rename project-name))
    (unless (equal (alist-get 'name (tab-bar--current-tab)) project-name)
      (tab-bar-select-tab-by-name project-name))))

(defun dw/project-prompter ()
  "Prompt for project and ensure it opens in the correct tab."
  (let ((project-dir (project-prompt-project-dir)))
    (dw/ensure-project-in-tab project-dir)
    project-dir))

;; Use our custom prompter for all project commands
(setq project-prompter #'dw/project-prompter)

(use-package eglot
  :ensure nil
  :hook ((js-mode
          typescript-mode) . eglot-ensure))

(use-package sigil
  :ensure nil
  :demand t
  :load-path "/home/daviwil/Projects/Code/sigil-worktrees/sigil-emacs/editor/emacs"
  :custom
  (sigil-repl-program "/home/daviwil/Projects/Code/sigil-worktrees/sigil-emacs/build/bin/sigil"))

(use-package js-mode
  :ensure nil
  :mode ("\\.jsx?\\'")
  :config
  (setq-default js-indent-level 2))

(use-package typescript-mode
  :mode ("\\.tsx?\\'")
  :config
  (setq-default typescript-indent-level 2))

(use-package janet-mode
  :ensure nil)

(provide 'dw-develop)
