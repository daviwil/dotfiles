;; -*- lexical-binding: t; -*-

(setup (:pkg zig-mode :straight t)
  (:disabled)
  (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
  (:load-after lsp-mode
    (lsp-register-client
      (make-lsp-client
        :new-connection (lsp-stdio-connection "~/Projects/Code/zls/zig-cache/bin/zls")
        :major-modes '(zig-mode)
        :server-id 'zls))))

(provide 'dw-dev-misc)
