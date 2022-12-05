;; -*- lexical-binding: t; -*-

(setup (:pkg ledger-mode)
  (:file-match "\\.lgr\\'")
  (:bind "TAB" completion-at-point)
  (:option
   ledger-reports '(("bal" "%(binary) -f %(ledger-file) bal")
                    ("bal this quarter" "%(binary) -f %(ledger-file) --period \"this quarter\" bal")
                    ("bal last quarter" "%(binary) -f %(ledger-file) --period \"last quarter\" bal")
                    ("reg" "%(binary) -f %(ledger-file) reg")
                    ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
                    ("account" "%(binary) -f %(ledger-file) reg %(account)"))))

(provide 'dw-finance)
