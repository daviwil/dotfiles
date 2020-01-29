(require 'map) ;; Needed for map-merge

(setq dw/system-settings
  (map-merge
    'list
    '((desktop/dpi . 180)
      (polybar/height . 35)
      (polybar/font-0-size . 18)
      (polybar/font-1-size . 14)
      (polybar/font-2-size . 20)
      (polybar/font-3-size . 13)
      (dunst/font-size . 20)
      (dunst/max-icon-size . 64)
      (vimb/default-zoom . 180))
    
    (when (equal system-name "zerocool"))
    
    (when (equal system-name "davinci")
      '((desktop/dpi . 130)
        (polybar/height . 25)
        (polybar/font-0-size . 12)
        (polybar/font-1-size . 8)
        (polybar/font-2-size . 14)
        (polybar/font-3-size . 9)
        (dunst/font-size . 14)
        (dunst/max-icon-size . 64)
        (vimb/default-zoom . 150)))
    ))
