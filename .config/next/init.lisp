(in-package :next)

(defvar *dw-keymap* (make-keymap)
  "Keymap for `dw-mode'.")

(define-mode dw-mode ()
  "Custom mode for the custom key bindings in `*dw-keymap*'."
  ((keymap-schemes :initform (list :emacs-map *dw-keymap*
                                   :vi-normal *dw-keymap*))))

;; Gimmie that Qutebrowser feel
(define-key :keymap *dw-keymap* "C-d" #'scroll-page-down)
(define-key :keymap *dw-keymap* "C-u" #'scroll-page-up)
(define-key :keymap *dw-keymap* "J" #'switch-buffer-previous)
(define-key :keymap *dw-keymap* "K" #'switch-buffer-next)

(add-to-default-list 'vi-normal-mode 'buffer 'default-modes)
(add-to-default-list 'dw-mode 'buffer 'default-modes)

(defun my-minibuffer-defaults (minibuffer)
  (setf (minibuffer-style minibuffer)
        (cl-css:css
            '((* :font-family "monospace,monospace")
              (body :border-top "4px solid dimgray"
                    :margin "0"
                    :padding "0 6px"
                    :background-color "#1c1f26")
              ("#container" :display "flex"
                            :flex-flow "column"
                            :height "100%")
              ("#input" :padding "6px 0"
                        :border-bottom "solid 1px lightgray")
              ("#completions" :flex-grow "1"
                              :overflow-y "auto"
                              :overflow-x "auto"
                              :background-color "#1c1f26"
                              :color "white")
              ("#cursor" :background-color "gray"
                         :color "white")
              ("#prompt" :padding-right "4px"
                         :color "lightgreen"
                         :background-color "#1c1f26")
              (ul :list-style "none"
                  :padding "0"
                  :margin "0")
              (li :padding "2px")
              (.marked :background-color "darkgray"
                       :font-weight "bold"
                       :color "white")
                ;; .selected must be set _after_ .marked so
                ;; that it overrides its attributes since the
                ;; candidate can be both marked and selected.
              (.selected :background-color "gray"
                         :color "white")))))

  ;; TODO: How do I set this?
  ;; (setf (minibuffer-line-style minibuffer)
  ;;   (cl-css:css
  ;;     `((* :font-size ,(minibuffer-font-size minibuffer)
  ;;          :line-height ,(minibuffer-line-height minibuffer)
  ;;          :color "white"
  ;;          :background-color "#1c1f26")))))

(defun my-buffer-defaults (buffer)
  (setf (zoom-ratio-default buffer) 1.6)
  (unzoom-page :buffer buffer))          ; Needed for non-web-mode buffers.

(defun my-interface-defaults ()
  (hooks:add-to-hook (hooks:object-hook *interface* 'minibuffer-make-hook)
                     #'my-minibuffer-defaults)
  (hooks:add-to-hook (hooks:object-hook *interface* 'buffer-make-hook)
                     #'my-buffer-defaults))

(hooks:add-to-hook '*after-init-hook* #'my-interface-defaults)
