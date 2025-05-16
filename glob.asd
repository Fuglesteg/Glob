(asdf:defsystem glob
  :author "Andreas Fuglesteg Dale <andreasfdale@gmail.com>"
  :license "GPL3"
  :depends-on (:slither)
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "glob"
  :entry-point "glob:start-glob"
  :components ((:file "glob")))