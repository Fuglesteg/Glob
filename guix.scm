(use-modules (guix packages)
             (guix gexp)
             (guix build-system asdf)
             (gnu packages lisp-xyz)
             ((guix licenses) #:prefix licenses:))

(define glob
  (package
   (name "glob")
   (version "0.1")
   (source (local-file (dirname (current-filename)) #:recursive? #t))
   (build-system asdf-build-system/sbcl)
   (inputs (list (load "../slither/guix.scm")
                 sbcl-deploy))
   (synopsis "") (description "") (license licenses:gpl3+) (home-page "")))

glob