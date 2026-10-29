
(asdf:defsystem #:maximilian-utils
  :description "Various utility functions and macros."
  :author "Maximilian Ballard"
  :license "GPLv3"
  :version "2.2.1"
  :depends-on ("uiop")
  :serial t
  :components ((:module "utils"
                :components ((:file "package")
                             ;; functions
                             (:file "misc")
                             (:file "time")
                             (:file "predicates")
                             (:file "string-manipulation")
                             ;; macros
                             (:file "macros")
                             (:file "struct-macros")
                             (:file "clos-macros")
                             (:file "memoize")))
               (:module "data-structures"
                :components ((:file "package")
                             (:file "queue"))))

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
                :components ((:file "package")
                             (:file "main")
                             (:file "utils")
                             (:file "utils-split-chars")
                             (:file "utils-case-compare-number")
                             (:file "utils-struct-macros")
                             (:file "utils-clos-macros")
                             (:file "utils-memoize")
                             (:file "data-structures"))))
  :description "Testing maximilian-utils"
  :perform (test-op (o c) (uiop:symbol-call :fiveam :run!
                                            (uiop:find-symbol* :main-suite :maximilian-utils/tests))))
