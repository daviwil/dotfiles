(define-module (daviwil home-services emacs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu home services)
  #:use-module (gnu home services utils)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)

  #:export (home-emacs-service-type
            home-emacs-configuration
	    home-emacs-module-configuration))

(define (serialize-module-name field-name value)
  (format #f "(require '~a)\n" value))

(define (wrap-module-body module-name value)
  (format #f "~a\n\n(provide '~a)" value module-name))

(define list-of-packages? (list-of package?))

(define-configuration home-emacs-module-configuration
  (packages
   (list-of-packages '())
   "Guix packages that this Emacs module depends on."
   empty-serializer)
  (module-name
   (string "unknown")
   "Emacs module name to `require'."
   serialize-module-name)
  (module-body
   (string "")
   "A file-like object containing the body of the module"
   empty-serializer))

(define list-of-emacs-modules? (list-of home-emacs-module-configuration?))

(define (serialize-emacs-modules field-name value)
  #~(string-append
     #$@(map
	 (cut serialize-configuration <> home-emacs-module-configuration-fields)
             value)))

(define-configuration home-emacs-configuration
  (package
    (package emacs)
    "Emacs package to use.")
  (modules
   (list-of-emacs-modules '())
   "Emacs configuration modules to include in init.el."
   serialize-emacs-modules))

(define (home-emacs-profile-service config)
  (cons (home-emacs-configuration-package config)
	(concatenate (map home-emacs-module-configuration-packages (home-emacs-configuration-modules
 config)))))

(define (home-emacs-files-service config)
  (cons
   (list
    ".config/emacs/init.el"
    (mixed-text-file "init.el" "(add-to-list 'load-path \"~/.config/emacs/modules\")" (serialize-configuration config home-emacs-configuration-fields)))
   (map
    (lambda (module)
      (let*((module-name (home-emacs-module-configuration-module-name module))
	    (file-name (format #f "~a.el" module-name)))
	(list
	 (string-append ".config/emacs/modules/" file-name)
	 (mixed-text-file
	  file-name
	  (wrap-module-body module-name (home-emacs-module-configuration-module-body module))))))
    (home-emacs-configuration-modules config))))

(define home-emacs-service-type
  (service-type (name 'home-emacs)
                (description "Emacs Configuration")
		(extensions
                 (list (service-extension
                        home-profile-service-type
                        home-emacs-profile-service)
                       (service-extension
                        home-files-service-type
                        home-emacs-files-service)))))

;; Example of configuration

(define emacs-module-completions
  (home-emacs-module-configuration
   (packages (list emacs-vertico))
   (module-name "completions")
   (module-body
    "
;;; Vertico
(require 'vertico)

;; Start Vertico
(vertico-mode 1)")))


(define emacs-module-appearance
  (home-emacs-module-configuration
   (packages (list emacs-vertico))
   (module-name "appearance")
   (module-body
    "
;;; Theme
(load-theme 'modus-vivendi)"
    )))

(define my-emacs-configuration
  (home-emacs-configuration
   (package emacs-next)
   (modules (list
	     emacs-module-appearance
	     emacs-module-completions))))

