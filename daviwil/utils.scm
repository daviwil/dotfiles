(define-module (daviwil utils)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (ice-9 pretty-print)

  #:export (gather-manifest-packages))

(define (read-manifest manifest-path)
  (with-input-from-file manifest-path
    (lambda ()
      (read))))

(define (gather-manifest-packages manifests)
  (if (pair? manifests)
      (begin
        (let ((manifest (read-manifest (string-append
                                        "/home/daviwil/.dotfiles/.files/.config/guix/manifests/"
                                        (symbol->string (car manifests))
                                        ".scm"))))
          ;; (pretty-print (eval manifest (current-module)))
          (append (map specification->package+output
                       (cadadr manifest))
                  (gather-manifest-packages (cdr manifests)))))
      '()))
