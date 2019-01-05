;; Source functions from stumpwm package
(in-package :stumpwm)

;; Load Quicklisp
(load "~/quicklisp/setup.lisp")

;; Load some modules
(load-module "ttf-fonts")
(load-module "battery")

;; Set StumpWM UI font
(xft:cache-fonts)
;; (set-font  "-*-lucidatypewriter-bold-r-*-*-0-120-*-*-*-*-*-*")
(set-font (make-instance 'xft:font :family "Fira Mono" :subfamily "Regular" :size 14))

;; Change focus to hovered windows
(setf *mouse-focus-policy* :sloppy)

;; Set modeline format
;; TODO: Does this work?
(setf *screen-mode-line-format*
    (list
     "%g "
     ;;"%w | "
     "^>" ;; Right-align rest of line
     ;TODO Re-enable!
     ;"%m | "
     ;;"%B | "
     ;;"%I | "
     "^B^7*" '(:eval (run-shell-command "date +\"%A %b %e, %I:%M %P\"" t))))
     ;; cpu | mem | battery
     ;;" | %D | %c %C (%f) %t | %M | %B"
     ;; " | %B"))

     ;; ip and gw
     ;;" | " '(:eval (current-ip)) " " '(:eval (current-gw)) ; defined above
     ;; net | wifi
     ;;" | %l | %I"
     ;;'(:eval (run-shell-command "echo" t))
     ;;"^2*" "[^B%n^b] %W%48"))

;; Set the group format string
(setf *group-format* "%n: %t")

;; Set modeline options
(setf *mode-line-timeout* 3) ;; in seconds
(setf *mode-line-foreground-color* "dodgerblue")
(setf *mode-line-background-color* "gray5")
(setf *mode-line-border-color* "dimgray")
(setf *mode-line-border-width* 2)
(setf *mode-line-pad-x* 3)
(setf *mode-line-pad-y* 3)

;; Set bar options
(set-fg-color "lightblue")

;; Turn on the modeline
(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))

;; Root prefix binding
(set-prefix-key (kbd "C-z"))

;; Window focus/movement keys
;; - Good reference for key bindings: https://github.com/alezost/stumpwmrc/blob/master/keys.lisp
(define-key *top-map* (kbd "s-Left") "move-focus left")
(define-key *top-map* (kbd "s-Right") "move-focus right")
(define-key *top-map* (kbd "s-Up") "move-focus up")
(define-key *top-map* (kbd "s-Down") "move-focus down")
(define-key *top-map* (kbd "s-S-Left") "move-window left")
(define-key *top-map* (kbd "s-S-Right") "move-window right")
(define-key *top-map* (kbd "s-S-Up") "move-window up")
(define-key *top-map* (kbd "s-S-Down") "move-window down")
(define-key *top-map* (kbd "s-q") "fselect")
(define-key *top-map* (kbd "s-Q") "kill")
(define-key *top-map* (kbd "s-TAB") "frame-windowlist")
(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-r") "iresize")
(define-key *top-map* (kbd "s-R") "loadrc")

;; Group keys
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")
(define-key *top-map* (kbd "s-!") "gmove 1")
(define-key *top-map* (kbd "s-@") "gmove 2")
(define-key *top-map* (kbd "s-#") "gmove 3")
(define-key *top-map* (kbd "s-$") "gmove 4")
(define-key *top-map* (kbd "s-%") "gmove 5")

;; Utility keys
(define-key *top-map* (kbd "s-d") "exec rofi -show drun -modi drun,run -sidebar-mode")
(define-key *top-map* (kbd "s-SPC") "exec rofi -show combi -modi combi,run -combi-modi window,drun -sidebar-mode")
(define-key *top-map* (kbd "s-L") "exec i3lock -c 001122")
(define-key *top-map* (kbd "s-E") "quit")

;; Set the terminal hotkey
(define-key *top-map* (kbd "s-RET") "exec xfce4-terminal")

;; Make the frame enumeration 1-based rather than 0-based
(setf *frame-number-map* "1234567890")

;; Control placement of StumpWM UI
(setf *message-window-gravity* :center)
(setf *input-window-gravity* :bottom)
(setf *timeout-wait* 3)
