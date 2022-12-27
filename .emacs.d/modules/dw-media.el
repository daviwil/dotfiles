;; -*- lexical-binding: t; -*-

(setup (:pkg mpv :straight t))

(setup (:pkg emms)
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (emms-mode-line-disable)
  (setq emms-source-file-default-directory "~/Music/")
  (dw/leader-key-def
    "am"  '(:ignore t :which-key "media")
    "amp" '(emms-pause :which-key "play / pause")
    "amf" '(emms-play-file :which-key "play file")))

(provide 'dw-media)
