(require 'org)

;; Don't ask when evaluating code blocks
(setq org-confirm-babel-evaluate nil)

(let* ((dotfiles-path (expand-file-name "~/.dotfiles"))
       (org-files (directory-files dotfiles-path nil "\\.org$")))
  (dolist (org-file org-files)
    (unless (equal org-file "README.org")
      (message "\n\033[1;32mUpdating %s\033[0m\n" org-file)
      (org-babel-tangle-file (expand-file-name org-file dotfiles-path)))))
