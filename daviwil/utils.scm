(define-module (daviwil utils)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)

  #:export (gather-manifest-packages
            apply-template-file))

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


(define (apply-template template-string value-alist)
  (regexp-substitute/global #f
                            "\\$\\{([A-Za-z/\\-]+)\\}"
                            template-string
                            'pre
                            (lambda (m)
                              (let ((entry (assq (string->symbol (match:substring m 1))
                                                 value-alist)))
                                (if entry
                                    (cdr entry)
                                    "VALUE NOT FOUND")))
                            'post))

(define (apply-template-file file-path value-alist)
  (call-with-input-file file-path
    (lambda (port)
      (apply-template (get-string-all port)
                      value-alist))))
