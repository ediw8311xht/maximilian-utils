
(asdf:defsystem #:maximilian-utils
  :description "Various utility functions and macros."
  :author "Maximilian Ballard"
  :license "GPLv3"
  :version "1.1"
  :depends-on ("uiop")
  :serial t
  :components ((:file "package")
               (:file "helpers")))
