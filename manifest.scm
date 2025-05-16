(use-modules (guix packages)
             (gnu packages lisp)
             (gnu packages lisp-xyz))

(define glob (load "./guix.scm"))

(packages->manifest (append (filter package?
                                    (map cadr
                                         (package-development-inputs glob)))
                            (list
                              sbcl
                              sbcl-micros)))
