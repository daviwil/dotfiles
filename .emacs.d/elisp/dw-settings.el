
(defun dw/load-system-settings ()
  (interactive)
  (load-file "~/.dotfiles/.emacs.d/per-system-settings.el"))

(defun dw/system-settings-get (setting)
  (alist-get setting dw/system-settings))

(provide 'dw-settings)
