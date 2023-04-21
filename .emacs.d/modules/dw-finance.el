;; -*- lexical-binding: t; -*-

(use-package ledger-mode
  :mode "\\.lgr\\'"
  :bind (:map ledger-mode-map
              ("TAB" . completion-at-point))

  :custom
  (ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
                    ("bal this quarter" "%(binary) -f %(ledger-file) --period \"this quarter\" bal")
                    ("bal last quarter" "%(binary) -f %(ledger-file) --period \"last quarter\" bal")
                    ("reg" "%(binary) -f %(ledger-file) reg")
                    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                    ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(provide 'dw-finance)
