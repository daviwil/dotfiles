(define-module (daviwil home-services emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu home services)
  #:use-module (gnu home services utils)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)

  #:export (home-emacs-service-type
            home-emacs-configuration))

(define (serialize-boolean field-name val)
  (if val "t" "nil"))

(define (serialize-field field-name val)
  (cond
   ((boolean? val) (serialize-boolean field-name val))
   (else
    (format #f "(~a . \"~a\")" field-name val))))

(define serialize-string serialize-field)

;; (define (serialize-emacs-vars field-name val)
;;   #~(string-append
;;      "(setq "
;;      #$@(map
;;          (match-lambda
;;            ((key . #f)
;;             #~(string-append #$key " nil\n"))
;;            ((key . #t)
;;             #~(string-append #$key " t\n"))
;;            ((key . value)
;;             #~(string-append #$key " " #$value "\n"))
;;            (_ ""))
;;          val)
;;      ")"))

(define (serialize-module-name field-name value)
  (format #f "(require '~a)\n" value))

(define (serialize-emacs-module module)
  #~(format #f "(require '~a)\n" #$(home-emacs-module-configuration-module-name module)))

(define (serialize-emacs-modules field-name value)
  #~(string-append
     ;; #$@(map serialize-emacs-module value)
     #$(serialize-emacs-module (car value))
     ))

(serialize-emacs-modules 'foo (home-emacs-configuration-modules test-config))
(map serialize-emacs-module (home-emacs-configuration-modules test-config))

(define list-of-packages? (list-of package?))

(define-configuration home-emacs-module-configuration
  ;; (packages
  ;;  (list-of-packages #f)
  ;;  "Guix packages that this Emacs module depends on."
  ;;  no-serialization)
  ;; (variables
  ;;  (alist '())
  ;;  "Emacs variables to set before requiring the package."
  ;;  serialize-emacs-vars)
  (module-name
   (string "unknown")
   "Emacs module name to `require'."
   serialize-module-name))

(define list-of-emacs-modules? (list-of home-emacs-module-configuration?))

(define-configuration home-emacs-configuration
  (package
    (package emacs)
    "Emacs package to use.")
  (modules
   (list-of-emacs-modules '())
   "Emacs configuration modules to include in init.el."
   serialize-emacs-modules)
  ;; (early-modules
  ;;  (list-of-emacs-modules '())
  ;;  "Emacs configuration modules to include in early-init.el."
  ;;  serialize-emacs-modules)
  (extra-init
   (text-config '())
   "Extra configuration text or files to include at the beginning of init.el")
  (extra-early-init
   (text-config '())
   "Extra configuration text or files to include at the beginning of early-init.el"))

(define (home-emacs-profile-service config)
  (list (home-emacs-configuration-package config)))

(define (home-emacs-files-service config)
  `((".config/emacs/init.el" ,(mixed-text-file "init.el"
                                               (home-emacs-configuration-extra-init config)
                                               (serialize-configuration config)
                                               "(message \"It works!\")"))))
(define test-config
  (home-emacs-configuration (modules (list (home-emacs-module-configuration
                                            (module-name "dw-core"))
                                           (home-emacs-module-configuration
                                            (module-name "dw-dev"))
                                           (home-emacs-module-configuration
                                            (module-name "dw-streaming"))))
                            (extra-init `(,(plain-file "extra-init" ";; Foo!")))))

(define init-el-file
  (mixed-text-file "init.el"
                   (serialize-configuration
                    test-config
                    (filter-configuration-fields
                     home-emacs-configuration-fields '(modules extra-init extra-early-init)))))

(define home-emacs-service-type
  (service-type (name 'home-emacs)
                (description "Emacs Configuration")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-emacs-profile-service)
                       (service-extension
                        home-files-service-type
                        home-emacs-files-service)))
                (default-value (home-emacs-configuration))))
