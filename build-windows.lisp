(require "asdf")
(require "uiop")

(load "~/.sbclrc")

(ql:quickload "deploy")
(asdf:load-asd (merge-pathnames "glob.asd" (uiop:getcwd)))
(ql:quickload "slither")
(asdf:make "glob")
