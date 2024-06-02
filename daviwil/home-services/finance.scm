(define-module (daviwil home-services finance)

  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages finance)
  #:use-module (gnu packages emacs-xyz)
  #:export (home-finance-service-type))

(define (home-finance-profile-service config)
  (list hledger
        emacs-ledger-mode))

(define home-finance-service-type
  (service-type (name 'home-finance)
                (description "Tools for finance management.")
                (extensions
                 (list (service-extension
                        home-profile-service-type
                        home-finance-profile-service)))
                (default-value #f)))
