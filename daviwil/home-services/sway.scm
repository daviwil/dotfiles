(define-module (daviwil home-services sway)
  #:use-module (ice-9 match)
  #:use-module (gnu home services sway))

(define (make-primary-outputs identifiers)
  (map (lambda (identifier)
         (sway-output
          (identifier identifier)
          (extra-content '("scale 2"))))
       identifiers))

(define workspace-list
  '((ws0 "0" "$laptop")
    (ws2 "2" "$primary $laptop")
    (ws3 "3" "$laptop")
    (ws4 "4" "$laptop")
    (ws5 "5" "$laptop")
    (ws1 "1" "$primary $laptop")))

(define* (my-sway-config #:key
                         (primary-outputs '("DP-1"))
                         (bg-path "~/.dotfiles/backgrounds/samuel-ferrara-uOi3lg8fGl4-unsplash.jpg"))
  (service home-sway-service-type
           (sway-configuration
            (variables
             (append
              `((laptop . "eDP-1")
                (primary . ,(string-join primary-outputs " ")))
              (map (match-lambda
                     ((ws num unused)
                      (cons ws num)))
                   workspace-list)
              %sway-default-variables))

            (extra-content
             `("font pango:JetBrains Mono 7"
               "floating_modifier $mod"
               ,@(map (match-lambda
                        ((ws num outputs)
                         (format #f "workspace $~a output ~a" ws outputs)))
                      workspace-list)))

            (outputs
             (append
              (make-primary-outputs primary-outputs)
              (list
               (sway-output
                (identifier 'eDP-1)
                (position (point (x 1920)
                                 (y 135)))
                (extra-content '("scale 2")))
               (sway-output
                (identifier '*)
                (background `(,bg-path . fill))))))

            (inputs
             (list (sway-input
                    (identifier "type:keyboard")
                    (layout (keyboard-layout "us,gr" #:options '("ctrl:nocaps"))))
                   (sway-input
                    (identifier "type:touchpad")
                    (tap #t)
                    (disable-while-typing #t)
                    (extra-content '("middle_emulation enabled")))))

            (gestures '())

            (keybindings
             `(($mod+Shift+q . "kill")
               ($mod+Shift+x . "exit")
               ($mod+Shift+r . "reload")
               ($mod+Mod1+h . "move workspace to output left")
               ($mod+Mod1+l . "move workspace to output right")
               ($mod+f . "fullscreen toggle")
               ($mod+Shift+f . "floating toggle")
               ($mod+Shift+p . "sticky toggle")
               ($mod+Control+space . "focus mode_toggle")
               ($mod+space . ,#~(string-append "exec "
                                               #$fuzzel
                                               "/bin/fuzzel"
                                               " -w 50 -x 8 -y 8 -r 3 -b 232635ff -t A6Accdff -s A6Accdff -S 232635ff -C c792eacc -m c792eacc -f \"JetBrains Mono:weight=light:size=10\" --icon-theme=\"Papirus-Dark\" --no-exit-on-keyboard-focus-loss"))))

            (startup-programs '())

            (packages
             (list swayidle
                   swaylock
                   fuzzel
                   wl-clipboard
                   mako
                   foot
                   gammastep
                   grimshot ;; grimshot --notify copy area
                   network-manager-applet)))))
