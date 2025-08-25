;; Monet adds some nice integrations into Emacs, like in-editor diffs
;; of proposed changes and editor context shared when sending prompts

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :rev :newest))

;; NOTE: This will take a while due to massive GIFs in the repo!
(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :bind  (:repeat-map my-claude-code-map
                      ("M" . claude-code-cycle-mode))
  :config
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode))

(provide 'dw-ai)
