#!/usr/bin/env -S guix shell -Df guix.scm -- sbcl --script

(require "asdf")
(require "uiop")
(require "deploy")

(asdf:load-asd "/home/andy/code/glob/glob.asd")
;(asdf:make :glob)
(deploy:shrinkwrap "glob")
