
(asdf:defsystem #:maximilian-utils
  :description "Various utility functions and macros."
  :author "Maximilian Ballard"
  :license "GPLv3"
  :version "2.1.2"
  :depends-on ("uiop")
  :serial t
  :components (
               (:module "utils"
                :components ((:file "package")
                             (:file "macros")
                             (:file "functions")))

               (:module "data-structures"
                :components ((:file "package")
                             (:file "queue")))
               )

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
                :components (
                             (:file "package")
                             (:file "utils")
                             (:file "utils-split-chars")
                             (:file "data-structures")
                             )))
  :description "Testing maximilian-utils"
  :perform (test-op (o c) (symbol-call :fiveam '#:run-all-tests)))
