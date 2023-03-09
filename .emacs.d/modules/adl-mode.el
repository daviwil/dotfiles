(defvar adl-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defconst adl--font-lock-keywords
  '(("^\\s-*\\(namespace\\|op\\|model\\|import\\|using\\|interface\\)\\s-+" . font-lock-keyword-face)
    ("\\s-*\\(mixes\\)\\s-+" . font-lock-keyword-face)
    ("\\s-*//.*$" . font-lock-comment-face) ;; This doesn't override others!
    ("@\\w+" . font-lock-function-name-face)
    ("byte\\|int32\\|int64\\|safeint\\|float32\\|float64\\|string\\|plainDate\\|plainTime\\|zonedDateTime\\|boolean\\|null" . font-lock-type-face)
    ("\\(-\\)?[0-9]+\\(\\.[0-9]+\\)?" . font-lock-constant-face)))

(define-derived-mode adl-mode
  prog-mode "ADL"
  "Major mode for the ADL language."
  :group 'adl
  (setq-local indent-line-function #'js-indent-line)
  (setq-local js-indent-level 2)
  (setq-local font-lock-defaults (list adl--font-lock-keywords))

  ;; Ignore comments in regex strings
  (setq-local parse-sexp-ignore-comments t)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  (setq-local comment-multi-line t)
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars))
  (setq-local electric-layout-rules
              '((?\; . after) (?\{ . after) (?\} . before))))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("adl-server" "--stdio"))
                    :major-modes '(adl-mode)
                    :server-id 'adl))
  (add-to-list 'lsp-language-id-configuration '(adl-mode . "cadl")))

(define-skeleton adl-skeleton-resource-update-model
  "Define a resource update model."
  "Model name: "
  "model " str "Update is ResourceUpdateModel<" str "Properties> {\n}")

(define-abbrev adl-mode-abbrev-table "rup" "" 'adl-skeleton-resource-update-model)

(provide 'adl-mode)
