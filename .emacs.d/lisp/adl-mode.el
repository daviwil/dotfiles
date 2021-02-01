
(defvar adl-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(define-derived-mode adl-mode
  prog-mode "ADL"
  "Major mode for the ADL language."
  :group 'adl
  (setq-local indent-line-function #'js-indent-line)
  (setq-local beginning-of-defun-function #'js-beginning-of-defun)
  (setq-local end-of-defun-function #'js-end-of-defun)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local font-lock-defaults
              (list js--font-lock-keywords nil nil nil nil
                    '(font-lock-syntactic-face-function
                      . js-font-lock-syntactic-face-function)))
  (setq-local syntax-propertize-function #'js-syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-multiline 'append 'local)
  (add-hook 'syntax-propertize-extend-region-functions
            #'js--syntax-propertize-extend-region 'append 'local)
  (setq-local prettify-symbols-alist js--prettify-symbols-alist)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local which-func-imenu-joiner-function #'js--which-func-joiner)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")
  (setq-local fill-paragraph-function #'js-fill-paragraph)
  (setq-local normal-auto-fill-function #'js-do-auto-fill)

  ;; Parse cache
  (add-hook 'before-change-functions #'js--flush-caches t t)

  ;; Imenu
  ;; (setq imenu-case-fold-search nil)
  ;; (setq imenu-create-index-function #'js--imenu-create-index)

  ;; for filling, pretend we're cc-mode
  ;; (c-init-language-vars js-mode)
  (setq-local comment-line-break-function #'c-indent-new-comment-line)
  (setq-local comment-multi-line t)
  (setq-local electric-indent-chars
        (append "{}():;," electric-indent-chars)) ;FIXME: js2-mode adds "[]*".
  (setq-local electric-layout-rules
        '((?\; . after) (?\{ . after) (?\} . before))))
