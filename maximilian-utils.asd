
(asdf:defsystem #:maximilian-utils
  :description "Various utility functions and macros."
  :author "Maximilian Ballard"
  :license "GPLv3"
  :version "1.1"
  :depends-on ("uiop")
  :serial t
  :components ((:file "package")
               (:file "main"))

  :description "some utilities"
  :in-order-to ((test-op (test-op "maximilian-utils/tests"))))

(asdf:defsystem #:maximilian-utils/tests
  :depends-on (
               :maximilian-utils
               :fiveam   ; testing framework
               :uiop     ; files
               :cl-ppcre ; checking for occurrences of string in output/file
               )
  :serial t
  :components ((:module "tests"
                :components ((:file "test-main")
                             (:file "test-helpers"))))
  :description "Testing maximilian-utils"
  :perform (test-op (o c) (symbol-call :fiveam '#:run-all-tests)))
