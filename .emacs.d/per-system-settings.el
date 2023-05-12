(require 'map) ;; Needed for map-merge

(setq dw/system-settings
  ;; (map-merge -- Something is broken with `map-into' in latest 28.1...
  (append
    ;; Put all system-specific settings at the front so that their values are
    ;; found first
    
    (when (equal system-name "acidburn")
      '((desktop/dpi . 180)
        (emacs/default-face-size . 105)
        (emacs/variable-face-size . 115)
        (emacs/fixed-face-size . 105)
        (polybar/height . 30)
        (polybar/font-0-size . 16)
        (polybar/font-1-size . 12)
        (polybar/font-2-size . 18)
        (polybar/font-3-size . 11)
        (dunst/font-size . 20)
        (dunst/max-icon-size . 88)
        (vimb/default-zoom . 160)
        (qutebrowser/default-zoom . 180)))
    
    (when (equal system-name "davinci")
      '((desktop/dpi . 130)
        (emacs/default-face-size . 165)
        (emacs/fixed-face-size . 165)
        (emacs/variable-face-size . 190)
        (polybar/height . 25)
        (polybar/font-0-size . 12)
        (polybar/font-1-size . 8)
        (polybar/font-2-size . 14)
        (polybar/font-3-size . 9)
        (dunst/font-size . 14)
        (dunst/max-icon-size . 64)
        (vimb/default-zoom . 150)
        (qutebrowser/default-zoom . 150)))
    
    ;; When booted into Windows
    (when (equal system-name "daviwil-t480")
      '((emacs/default-face-size . 110)
        (emacs/fixed-face-size . 110)
        (emacs/variable-face-size . 134)))
    
    (when (equal system-name "phantom")
      '((desktop/dpi . 220)
        (emacs/default-face-size . 240)
        (emacs/variable-face-size . 260)
        (emacs/fixed-face-size . 230)
        (polybar/height . 50)
        (polybar/font-0-size . 20)
        (polybar/font-1-size . 16)
        (polybar/font-2-size . 22)
        (polybar/font-3-size . 15)
        (qutebrowser/default-zoom . 300)
        (vimb/default-zoom . 200)))
    
    '((desktop/dpi . 180)
      (desktop/background . "samuel-ferrara-uOi3lg8fGl4-unsplash.jpg")
      (emacs/default-face-size . 110)
      (emacs/variable-face-size . 120)
      (emacs/fixed-face-size . 110)
      (polybar/height . 35)
      (polybar/font-0-size . 18)
      (polybar/font-1-size . 14)
      (polybar/font-2-size . 20)
      (polybar/font-3-size . 13)
      (dunst/font-size . 20)
      (dunst/max-icon-size . 88)
      (vimb/default-zoom . 180)
      (qutebrowser/default-zoom . 200))))
