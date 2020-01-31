(use-package exwm
  :init
  (setq mouse-autoselect-window nil
        focus-follows-mouse t
        exwm-workspace-warp-cursor t
        exwm-workspace-number 5)
        ;exwm-workspace-minibuffer-position 'bottom) ;; Annoying focus issues
  :config
  ;; Make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (string-equal exwm-class-name "Vimb")
                (exwm-workspace-rename-buffer (format "vimb: %s" exwm-title)))))

  (exwm-enable))

;; Enable exwm-randr before exwm-init gets called
(use-package exwm-randr
  :if dw/exwm-enabled
  :after (exwm)
  :config
  (exwm-randr-enable)
  (setq exwm-randr-workspace-monitor-plist '(4 "eDP-1")))

(defun exwm/run-in-background (command)
   (start-process-shell-command command nil
                                command))

(defun exwm/bind-function (key invocation &rest bindings)
  "Bind KEYs to FUNCTIONs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           ,invocation))
    (setq key (pop bindings)
          command
          (pop bindings))))

(defun exwm/bind-command (key command &rest bindings)
  "Bind KEYs to COMMANDs globally"
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (exwm/run-in-background ,command)))
    (setq key (pop bindings)
          command
          (pop bindings))))

(defun dw/exwm-init-hook ()
  ;; Launch Telega in workspace 0 if we've logged in before
  (when (file-exists-p "~/.telega/db.sqlite")
    (telega nil))

  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  (eshell)

  ;; Launch apps that will run in the background
  (exwm/run-in-background "dunst")
  (exwm/run-in-background "nm-applet")
  (exwm/run-in-background "QSyncthingTray")
  (exwm/run-in-background "redshift -l 47.675510:-122.203362 -t 6500:3500"))

(use-package exwm
  :if dw/exwm-enabled
  :config
  ;(display-time-mode 1) ;; Not needed for now since we have a panel

  (add-hook 'exwm-mode-hook
            (lambda ()
              (evil-local-set-key 'motion (kbd "C-u") nil)))

  (require 'dw-exwm)

  (defun dw/setup-window-by-class ()
    (interactive)
    (pcase exwm-class-name
      ("Pidgin" (exwm-workspace-move-window 0))
      ("Pidgin<2>" (exwm-workspace-move-window 0))
      ("teams-for-linux" (exwm-workspace-move-window 3))
      ("Microsoft Teams - Preview" (exwm-workspace-move-window 3))
      ("Spotify" (exwm-workspace-move-window 4))
      ("Vimb" (exwm-workspace-move-window 2))
      ("qjackctl" (exwm-floating-toggle-floating))
      ("mpv" (exwm-floating-toggle-floating)
             (dw/exwm-floating-toggle-pinned))))

  ;; Do some post-init setup
  (add-hook 'exwm-init-hook #'dw/exwm-init-hook)

  ;; Manipulate windows as they're created
  (add-hook 'exwm-manage-finish-hook
            (lambda ()
              ;; Send the window where it belongs
              (dw/setup-window-by-class)))

              ;; Hide the modeline on all X windows
              ;(exwm-layout-hide-mode-line)))

  ;; Hide the modeline on all X windows
  (add-hook 'exwm-floating-setup-hook
            (lambda ()
              (exwm-layout-hide-mode-line))))

(use-package exwm-systemtray
  :disabled
  :if dw/exwm-enabled
  :after (exwm)
  :config
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 35))

(defun dw/run-xmodmap ()
  (interactive)
  (start-process-shell-command "xmodmap" nil "xmodmap ~/.dotfiles/.config/i3/Xmodmap"))

(defun dw/update-wallpapers ()
  (interactive)
  (start-process-shell-command "feh" nil "feh --bg-scale ~/.dotfiles/backgrounds/mountains-1412683.jpg"))

(setq dw/panel-process nil)
(defun dw/kill-panel ()
  (interactive)
  (when dw/panel-process
    (ignore-errors
      (kill-process dw/panel-process)))
  (setq dw/panel-process nil))

(defun dw/start-panel ()
  (interactive)
  (dw/kill-panel)
  (setq dw/panel-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun dw/update-screen-layout ()
  (interactive)
  (let ((layout-script "~/.bin/update-screens"))
     (message "Running screen layout script: %s" layout-script)
     (start-process-shell-command "xrandr" nil layout-script)))

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

(defalias 'switch-to-buffer-original 'exwm-workspace-switch-to-buffer)
;; (defalias 'switch-to-buffer 'exwm-workspace-switch-to-buffer)

;; (defun dw/counsel-switch-buffer ()
;;   "Switch to another buffer.
;; Display a preview of the selected ivy completion candidate buffer
;; in the current window."
;;   (interactive)
;;   (ivy-read "Switch to buffer: " 'internal-complete-buffer
;;             :preselect (buffer-name (other-buffer (current-buffer)))
;;             :keymap ivy-switch-buffer-map
;;             :action #'ivy--switch-buffer-action
;;             :matcher #'ivy--switch-buffer-matcher
;;             :caller 'counsel-switch-buffer
;;             :unwind #'counsel--switch-buffer-unwind
;;             :update-fn 'counsel--switch-buffer-update-fn)
;; )

(defun dw/send-polybar-hook (name number)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" name number)))

(defun dw/update-polybar-exwm ()
  (dw/send-polybar-hook "exwm" 1))

(defun dw/update-polybar-telegram ()
  (dw/send-polybar-hook "telegram" 1))

(defun dw/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

(defun dw/polybar-mail-count (max-count)
  (if dw/mail-enabled
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
      ?\C-\     ;; Ctrl+Space
      ?\C-\;))

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (exwm/bind-command
    "s-p" "playerctl play-pause"
    "s-[" "playerctl previous"
    "s-]" "playerctl next")

  (use-package desktop-environment
    :after exwm
    :config (desktop-environment-mode)
    :custom
    (desktop-environment-brightness-small-increment "2%+")
    (desktop-environment-brightness-small-decrement "2%-")
    (desktop-environment-brightness-normal-increment "5%+")
    (desktop-environment-brightness-normal-decrement "5%-"))

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
  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
  (exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen))
