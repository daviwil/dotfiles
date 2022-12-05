;; -*- lexical-binding: t; -*-

(setup (:pkg posframe))
(setup (:pkg command-log-mode :straight t))

(setq dw/command-window-frame nil)

(defun dw/toggle-command-window ()
  (interactive)
  (if dw/command-window-frame
      (progn
        (posframe-delete-frame clm/command-log-buffer)
        (setq dw/command-window-frame nil))
      (progn
        (global-command-log-mode t)
        (with-current-buffer
          (setq clm/command-log-buffer
                (get-buffer-create " *command-log*"))
          (text-scale-set -1))
        (setq dw/command-window-frame
          (posframe-show
            clm/command-log-buffer
            :position `(,(- (x-display-pixel-width) 590) . 15)
            :width 38
            :height 5
            :min-width 38
            :min-height 5
            :internal-border-width 2
            :internal-border-color "#c792ea"
            :override-parameters '((parent-frame . nil)))))))

(dw/leader-key-def
 "tc" 'dw/toggle-command-window)

(defun dw/keycast-predicate ()
  ;; Don't show keys if we're in the minibuffer
  (not (minibufferp)))

(setup (:pkg keycast)
  ;; This works with doom-modeline, inspired by this comment:
  ;; https://github.com/tarsius/keycast/issues/7#issuecomment-627604064
  (require 'keycast)
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    ;; Don't
    (setq keycast-window-predicate #'dw/keycast-predicate)
    (if keycast-mode
        (add-hook 'pre-command-hook 'keycast--update)
      (remove-hook 'pre-command-hook 'keycast--update)))

  ;; Make the key face normal-sized
  (set-face-attribute 'keycast-key nil :height 1.0)

  ;; Make sure Keycast gets added at the front of the list
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (add-to-list 'global-mode-string '("" mode-line-keycast "       ")))))

(setup (:pkg obs-websocket :guix "emacs-obs-websocket-el")
  (require 'obs-websocket)
  (defhydra dw/stream-keys (:exit t)
    "Stream Commands"
    ("c" (obs-websocket-connect) "Connect")
    ("l" (obs-websocket-send "SetCurrentScene" :scene-name "Logo Screen") "Logo Screen" :exit nil)
    ("s" (obs-websocket-send "SetCurrentScene" :scene-name "Screen") "Screen")
    ("w" (obs-websocket-send "SetCurrentScene" :scene-name "Webcam") "Webcam")
    ("p" (obs-websocket-send "SetCurrentScene" :scene-name "Sponsors") "Sponsors")
    ("e" (obs-websocket-send "SetCurrentScene" :scene-name "Thanks For Watching") "Thanks For Watching")
    ("Ss" (obs-websocket-send "StartStreaming") "Start Stream")
    ("Se" (obs-websocket-send "StopStreaming") "End Stream"))

  ;; This is Super-s (for now)
  (global-set-key (kbd "s-s") #'dw/stream-keys/body))

(setup (:pkg request))
(setup (:pkg a))

(setup (:pkg live-crafter
             :host github
             :repo "SystemCrafters/live-crafter")
  (:load-after mpv))

(provide 'dw-streaming)
