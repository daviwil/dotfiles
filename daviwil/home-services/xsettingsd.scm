(define-module (daviwil home-services xsettingsd)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (daviwil packages fonts)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-xsettingsd-service-type
            home-xsettingsd-configuration))

(define-configuration home-xsettingsd-configuration
  (dpi
   (integer 180)
   "Screen dots per inch (DPI).")

  (theme
   (string "Adwaita")
   "The name of the GTK theme to apply.")

  (icon-theme
   (string "Adwaita")
   "The name of the icon theme to apply.")

  (font
   (string "Deja Vu Sans 10")
   "The name and size of the normal user interface font.")

  (monospace-font
   (string "Deja Vu Mono 10")
   "The name and size of the monospace user interface font.")

  (extra-content
   (string "")
   "Extra content appended as-is to the xsettingsd configuration file.")

  ;; Don't use the serialization system!
  (no-serialization))

(define (generate-xsettingsd-file config)
  (string-append "
Net/ThemeName \"" (home-xsettingsd-configuration-theme config) "\"
Net/IconThemeName \"" (home-xsettingsd-configuration-icon-theme config) "\"
Gtk/DecorationLayout \"menu:minimize,maximize,close\"
Gtk/FontName \"" (home-xsettingsd-configuration-font config) "\"
Gtk/MonospaceFontName \"" (home-xsettingsd-configuration-monospace-font config) "\"
Gtk/CursorThemeName \"Adwaita\"
Xft/DPI " (number->string (* 1024 (home-xsettingsd-configuration-dpi config))) " # 1024 * DPI"
"\n"
(home-xsettingsd-configuration-extra-content config)
"\n"))

(define (home-xsettingsd-files-service config)
  (list
   `(".config/xsettingsd/xsettingsd.conf" ,(plain-file "xsettingsd.conf"
                                                       (generate-xsettingsd-file config)))))

(define (home-xsettingsd-profile-service config)
  (map specification->package
       (list "xsettingsd")))


(define home-xsettingsd-service-type
  (service-type (name 'home-xsettingsd)
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-xsettingsd-profile-service)
                       (service-extension
                        home-files-service-type
                        home-xsettingsd-files-service)))
                (default-value (home-xsettingsd-configuration))
                (description "Configures UI appearance settings for Xorg sessions using the xsettingsd
daemon.")))
