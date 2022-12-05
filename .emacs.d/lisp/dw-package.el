;;; -- Install Straight.el -----

(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;;; -- Setup.el -----

(straight-use-package '(setup :type git :host nil :repo "https://git.sr.ht/~pkal/setup"))
(require 'setup)

;; Uncomment this for debugging purposes
;; (defun dw/log-require (&rest args)
;;   (with-current-buffer (get-buffer-create "*require-log*")
;;     (insert (format "%s\n"
;;                     (file-name-nondirectory (car args))))))
;; (add-to-list 'after-load-functions #'dw/log-require)

;; Recipe is always a list
;; Install via Guix if length == 1 or :guix t is present

(defvar dw/guix-emacs-packages '()
  "Contains a list of all Emacs package names that must be
installed via Guix.")

;; Examples:
;; - (org-roam :straight t)
;; - (git-gutter :straight git-gutter-fringe)

(defun dw/filter-straight-recipe (recipe)
  (let* ((plist (cdr recipe))
         (name (plist-get plist :straight)))
    (cons (if (and name (not (equal name t)))
              name
            (car recipe))
          (plist-put plist :straight nil))))

(setup-define :pkg
  (lambda (&rest recipe)
    (if (and dw/is-guix-system
             (or (eq (length recipe) 1)
                 (plist-get (cdr recipe) :guix)))
        `(add-to-list 'dw/guix-emacs-packages
                      ,(or (plist-get recipe :guix)
                           (concat "emacs-" (symbol-name (car recipe)))))
      `(straight-use-package ',(dw/filter-straight-recipe recipe))))
  :documentation "Install RECIPE via Guix or straight.el"
  :shorthand #'cadr)

(setup-define :delay
   (lambda (&rest time)
     `(run-with-idle-timer ,(or time 1)
                           nil ;; Don't repeat
                           (lambda () (require ',(setup-get 'feature)))))
   :documentation "Delay loading the feature until a certain amount of idle time has passed.")

(setup-define :disabled
  (lambda ()
    `,(setup-quit))
  :documentation "Always stop evaluating the body.")

(setup-define :load-after
    (lambda (features &rest body)
      (let ((body `(progn
                     (require ',(setup-get 'feature))
                     ,@body)))
        (dolist (feature (if (listp features)
                             (nreverse features)
                           (list features)))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
  :documentation "Load the current feature after FEATURES."
  :indent 1)

(setup-define :load-path
    (lambda (path)
      `(let ((path* (expand-file-name ,path)))
         (if (file-exists-p path*)
             (add-to-list 'load-path path*)
           ,(setup-quit))))
  :documentation "Add PATH to load path.
This macro can be used as NAME, and it will replace itself with
the nondirectory part of PATH.
If PATH does not exist, abort the evaluation."
  :shorthand (lambda (args)
               (intern
                (file-name-nondirectory
                 (directory-file-name (cadr args))))))

(provide 'dw-package)
