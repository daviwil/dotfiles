(define-module (daviwil packages kubernetes)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp))

(define-public k3d
  (package
    (name "k3d")
    (version "5.8.3")
    (source
     (origin
       (method url-fetch/executable)
       (uri (string-append "https://github.com/k3d-io/k3d/releases/"
                           "download/v" version "/k3d-linux-amd64"))
       (file-name "k3d")
       (sha256
        (base32 "0dhissffa5zz9klgbnxan5sp3vqvg8syvafdlgh0z67cgaw0rvdk"))))
    (build-system copy-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin-dir (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin-dir)
               (copy-file (assoc-ref inputs "source")
                          (string-append bin-dir "/k3d"))
               (chmod (string-append bin-dir "/k3d") #o755)))))))
    (home-page "https://github.com/k3d-io/k3d")
    (synopsis "Little helper to run CNCF's k3s in Docker")
    (description
     "k3d creates containerized k3s clusters. This means, that you can spin up a multi-node k3s cluster on a single machine using Docker.")
    (license license:expat)))

(define-public kubectl
  (package
    (name "kubectl")
    (version "1.32.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dl.k8s.io/release/v" version
                           "/bin/linux/amd64/kubectl"))

       (file-name "kubectl")
       (sha256
        (base32 "1shpz1liyk11i8j8p03bqhrjjajlkc8cfk1mz0sj2w2vrjfrasjg"))))
    (build-system copy-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin-dir (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin-dir)
               (copy-file (assoc-ref inputs "source")
                          (string-append bin-dir "/kubectl"))
               (chmod (string-append bin-dir "/kubectl") #o755)))))))
    (home-page "https://github.com/k3d-io/k3d")
    (synopsis "Little helper to run CNCF's k3s in Docker")
    (description
     "k3d creates containerized k3s clusters. This means, that you can spin up a multi-node k3s cluster on a single machine using Docker.")
    (license license:expat)))
