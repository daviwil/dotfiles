;; -*- lexical-binding: t; -*-

(use-package logos
  :bind (([remap narrow-to-region] . logos-narrow-dwim)
	 ([remap forward-page] . logos-forward-page-dwim)
         ([remap backward-page] . logos-backward-page-dwim))
  :custom
  (logos-outlines-are-pages t))

(provide 'dw-present)
