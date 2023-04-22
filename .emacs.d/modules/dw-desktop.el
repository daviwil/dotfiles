;; -*- lexical-binding: t; -*-

(use-package exwm
  :config
  (setq mouse-autoselect-window nil
        focus-follows-mouse t
        exwm-workspace-warp-cursor t
        exwm-workspace-number 5)
        ;exwm-workspace-display-echo-area-timeout 5
        ;exwm-workspace-minibuffer-position 'bottom) ;; Annoying focus issues

  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (pcase exwm-class-name
                ("Vimb" (exwm-workspace-rename-buffer (format "vimb: %s" exwm-title)))
                ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title))))))

  ;; Enable exwm-randr before exwm-init gets called
  (require 'exwm-randr)
  (exwm-randr-enable)
  (setq exwm-randr-workspace-monitor-plist
        (pcase system-name
          ("acidburn" '(0 "eDP-1" 1 "DP-3-3" 2 "DP-3-3" 3 "DP-3-3" 4 "eDP-1"))
          (_ nil)))

  ;; Set up EXWM, it will be initialized once loading is complete
  (exwm-enable)

  ;; Enable the system tray
  (require 'exwm-systemtray)
  (exwm-systemtray-enable))

(defun exwm/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun exwm/bind-function (key invocation &rest bindings)
  "Bind KEYs to FUNCTIONs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (funcall ',invocation)))
    (setq key (pop bindings)
          invocation (pop bindings))))

(defun exwm/bind-command (key command &rest bindings)
  "Bind KEYs to COMMANDs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (exwm/run-in-background ,command)))
    (setq key (pop bindings)
          command (pop bindings))))

(defun dw/exwm-init-hook ()
  (with-eval-after-load 'tab-bar
    ;; Set up perspective names on initial workspaces
    (exwm-workspace-switch-create 0)
    (tab-bar-rename-tab "Chat")

    ;; Launch Telega in workspace 0 if we've logged in before
    (when (file-exists-p "~/.telega/db.sqlite")
      (telega nil))

    (exwm-workspace-switch-create 1)
    (tab-bar-rename-tab "Main")

    (exwm-workspace-switch-create 2)
    (tab-bar-rename-tab "Browsers")

    (exwm-workspace-switch-create 2)
    (tab-bar-rename-tab "Browsers")

    (exwm-workspace-switch-create 3)
    (tab-bar-rename-tab "Comms")

    (exwm-workspace-switch-create 4)
    (tab-bar-rename-tab "Media")

    ;; Make sure tab-bar faces are set correctly
    (dw/set-tab-bar-faces)

    ;; Make workspace 1 be the one where we land at startup
    (exwm-workspace-switch-create 1))

  ;; Launch apps that will run in the background
  (exwm/run-in-background "dunst")
  (exwm/run-in-background "nm-applet")
  (exwm/run-in-background "syncthing-gtk --minimized")
  ;; Athens
  ;; (exwm/run-in-background "redshift -l 37.983810:23.727539 -t 6500:3500")
  ;; Fix -- xgamma -rgamma 0.9 -ggamma 1.0 -bgamma 1.0
  )
  ;; Seattle
  ;(exwm/run-in-background "redshift -l 47.675510:-122.203362 -t 6500:3500"))

(defun dw/setup-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Emacs" (call-interactively #'exwm-input-toggle-keyboard))
    ("Xephyr" (call-interactively #'exwm-input-toggle-keyboard))
    ("discord" (exwm-workspace-move-window 3))
    ("Microsoft Teams - Preview" (exwm-workspace-move-window 3))
    ("Spotify" (exwm-workspace-move-window 3))
    ("Vimb" (exwm-workspace-move-window 2))
    ("qutebrowser" (exwm-workspace-move-window 2))
    ("qjackctl" (exwm-floating-toggle-floating))
    ("mpv" (exwm-floating-toggle-floating)
           (dw/exwm-floating-toggle-pinned))
    ("gsi" (exwm-input-toggle-keyboard))))

(use-package exwm
  :config
  ;; Do some post-init setup
  (add-hook 'exwm-init-hook #'dw/exwm-init-hook)

  ;; Manipulate windows as they're created
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              ;; Send the window where it belongs
              (dw/setup-window-by-class)))

              ;; Hide the modeline on all X windows
              ;(exwm-layout-hide-mode-line)))

  ;; Hide the modeline on all floating windows
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (toggle-frame-tab-bar) ;; No tab bars on floating frames
              (exwm-layout-hide-mode-line))))

(defun dw/run-xmodmap ()
  (interactive)
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.dotfiles/.files/.config/i3/Xmodmap"))

(defun dw/update-wallpapers ()
  (interactive)
  (start-process-shell-command
   "feh" nil
   (format "feh --bg-scale ~/.dotfiles/backgrounds/%s" (alist-get 'desktop/background dw/system-settings))))

(setq dw/panel-process nil)
(defun dw/kill-panel ()
  (interactive)
  (when dw/panel-process
    (ignore-errors
      (kill-process dw/panel-process)))
  (setq dw/panel-process nil))

(defun dw/start-panel ()
  (interactive)
  (dw/kill-panel))
  ;(setq dw/panel-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun dw/update-screen-layout ()
  (interactive)
  (let ((layout-script "~/.bin/update-screens"))
     (message "Updating screen layout with autorandr..." layout-script)
     (start-process-shell-command "autorandr" nil "autorandr -c")))

(defun dw/configure-desktop ()
  (interactive)
    (dw/run-xmodmap)
    (dw/update-screen-layout)
    (run-at-time "2 sec" nil (lambda () (dw/update-wallpapers))))

(defun dw/on-exwm-init ()
  (dw/configure-desktop)
  (dw/start-panel))

(when dw/exwm-enabled
  ;; Configure the desktop for first load
  (add-hook 'exwm-init-hook #'dw/on-exwm-init))

(defun dw/send-polybar-hook (name number)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" name number)))

(defun dw/update-polybar-exwm (&optional path)
  (dw/send-polybar-hook "exwm" 1)
  (dw/send-polybar-hook "exwm-path" 1))

(defun dw/update-polybar-telegram ()
  (dw/send-polybar-hook "telegram" 1))

(defun dw/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

(defun dw/polybar-exwm-workspace-path ()
  (let ((workspace-path (frame-parameter nil 'bufler-workspace-path-formatted)))
    (if workspace-path
        (substring-no-properties workspace-path)
      "")))

(defun dw/polybar-mail-count (max-count)
  (if (and dw/mail-enabled dw/mu4e-inbox-query)
    (let* ((mail-count (shell-command-to-string
                         (format "mu find --nocolor -n %s \"%s\" | wc -l" max-count dw/mu4e-inbox-query))))
      (format " %s" (string-trim mail-count)))
    ""))

(defun dw/telega-normalize-name (chat-name)
  (let* ((trimmed-name (string-trim-left (string-trim-right chat-name "}") "◀{"))
         (first-name (nth 0 (split-string trimmed-name " "))))
    first-name))

(defun dw/propertized-to-polybar (buffer-name)
  (if-let* ((text (substring-no-properties buffer-name))
            (fg-face (get-text-property 0 'face buffer-name))
            (fg-color (face-attribute fg-face :foreground)))
    (format "%%{F%s}%s%%{F-}" fg-color (dw/telega-normalize-name text))
    text))

(defun dw/polybar-telegram-chats ()
  (if (> (length tracking-buffers) 0)
    (format " %s" (string-join (mapcar 'dw/propertized-to-polybar tracking-buffers) ", "))
    ""))

(add-hook 'exwm-workspace-switch-hook #'dw/update-polybar-exwm)
(add-hook 'bufler-workspace-set-hook #'dw/update-polybar-exwm)

(when dw/exwm-enabled
  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\M-k  ;; Browser list
      ?\C-\M-n  ;; Next workspace
      ?\C-\M-'  ;; Popper toggle
      ?\C-\     ;; Ctrl+Space
      ?\C-\;))

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (defun exwm/run-vimb ()
    (exwm/run-in-background "vimb")
    (exwm-workspace-switch-create 2))

  (defun exwm/run-qute ()
    (exwm/run-in-background "qutebrowser")
    (exwm-workspace-switch-create 2))

  (exwm/bind-function
    "s-o" 'exwm/run-qute
    "s-q" 'kill-buffer)

  (exwm/bind-command
    "s-p" "playerctl play-pause"
    "s-[" "playerctl previous"
    "s-]" "playerctl next")

  (use-package desktop-environment
    :config
    (desktop-environment-mode)

    :custom
    (desktop-environment-brightness-small-increment "2%+")
    (desktop-environment-brightness-small-decrement "2%-")
    (desktop-environment-brightness-normal-increment "5%+")
    (desktop-environment-brightness-normal-decrement "5%-")
    (desktop-environment-screenshot-command "flameshot gui"))

  ;; This needs a more elegant ASCII banner
  (defhydra hydra-exwm-move-resize (:timeout 4)
    "Move/Resize Window (Shift is bigger steps, Ctrl moves window)"
    ("j" (lambda () (interactive) (exwm-layout-enlarge-window 10)) "V 10")
    ("J" (lambda () (interactive) (exwm-layout-enlarge-window 30)) "V 30")
    ("k" (lambda () (interactive) (exwm-layout-shrink-window 10)) "^ 10")
    ("K" (lambda () (interactive) (exwm-layout-shrink-window 30)) "^ 30")
    ("h" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 10)) "< 10")
    ("H" (lambda () (interactive) (exwm-layout-shrink-window-horizontally 30)) "< 30")
    ("l" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 10)) "> 10")
    ("L" (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 30)) "> 30")
    ("C-j" (lambda () (interactive) (exwm-floating-move 0 10)) "V 10")
    ("C-S-j" (lambda () (interactive) (exwm-floating-move 0 30)) "V 30")
    ("C-k" (lambda () (interactive) (exwm-floating-move 0 -10)) "^ 10")
    ("C-S-k" (lambda () (interactive) (exwm-floating-move 0 -30)) "^ 30")
    ("C-h" (lambda () (interactive) (exwm-floating-move -10 0)) "< 10")
    ("C-S-h" (lambda () (interactive) (exwm-floating-move -30 0)) "< 30")
    ("C-l" (lambda () (interactive) (exwm-floating-move 10 0)) "> 10")
    ("C-S-l" (lambda () (interactive) (exwm-floating-move 30 0)) "> 30")
    ("f" nil "finished" :exit t))

  ;; Workspace switching
  (setq exwm-input-global-keys
         `(([?\s-\C-r] . exwm-reset)
           ([?\s-w] . exwm-workspace-switch)
           ([?\s-i] . exwm-input-toggle-keyboard)
           ([?\s-r] . hydra-exwm-move-resize/body)
           ([?\s-e] . dired-jump)
           ([?\s-E] . (lambda () (interactive) (dired "~")))
           ([?\s-Q] . (lambda () (interactive) (kill-buffer)))
           ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
           ,@(mapcar (lambda (i)
                       `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                           (interactive)
                           (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))

  (exwm-input-set-key (kbd "<s-return>") 'vterm)
  (exwm-input-set-key (kbd "s-SPC") 'app-launcher-run-app)
  (exwm-input-set-key (kbd "s-S") 'dw/configure-desktop)
  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen))

;; (use-package app-launcher)

;; Don't let ediff break EXWM, keep it in one frame
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(provide 'dw-desktop)
