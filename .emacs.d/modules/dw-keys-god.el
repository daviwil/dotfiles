(require 'god-mode)
(god-mode)

(global-set-key (kbd "<escape>") #'(lambda () (interactive) (god-local-mode 1)))

(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(define-key god-local-mode-map (kbd ".") #'repeat)
(global-set-key (kbd "C-x C-1") #'delete-other-windows)
(global-set-key (kbd "C-x C-2") #'split-window-below)
(global-set-key (kbd "C-x C-3") #'split-window-right)
(global-set-key (kbd "C-x C-0") #'delete-window)
(global-set-key (kbd "C-x C-r") #'eval-region) ;; doesn't work?

;; Other useful keys:
;; x j - dired-jump

(with-eval-after-load 'lispy
  (add-to-list 'lispy-compat 'god-mode))

(provide 'dw-keys-god)
