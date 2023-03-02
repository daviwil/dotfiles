(define-module (daviwil home-services template-file)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (daviwil utils)
  #:export (home-template-file-service-type
            home-template-file-configuration))

(define-configuration/no-serialization home-template-file-configuration
  (base-path
   (string)
   "The base path from which all relative paths are calculated.")
  (files
   (list-of-strings)
   "The list of relative file paths to include.")
  (values
   (alist)
   "The list of key-value pairs to use for templating the configuration files"))

(define (home-template-file-files-service config)
  (map (lambda (file)
         (let ((full-path (canonicalize-path (string-append (home-template-file-configuration-base-path config)
                                                            file-name-separator-string
                                                            file))))
           `(,file ,(plain-file
                          (basename full-path)
                          (apply-template-file full-path
                                               (home-template-file-configuration-values config))))))
       (home-template-file-configuration-files config)))

(define-public home-template-file-service-type
  (service-type (name 'home-template-file)
                (description "Creates configuration files based on templated text.")
                (extensions
                 (list (service-extension
                        home-files-service-type
                        home-template-file-files-service)))
                (default-value #f)))
