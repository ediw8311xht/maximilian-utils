
(in-package :maximilian-utils/tests)

(def-suite struct-macros-suite
  :description "utils testing"
  :in utils-suite)
(in-suite struct-macros-suite)

(defstruct-with-helpers (test-struct (:with-get-set slot))
  (a "init" :type string)
  (b 3      :type number))

(test defstruct-with-helpers
  (let ((obj (make-test-struct)))
    ;; Test standard creation
    (is (equal "init" (test-struct-a obj)))
    (is (= 3 (test-struct-b obj)))
    
    ;; Test the :with-get-set slot dispatcher macro
    (is (equal "init" (test-struct-slot :a obj)))
    (is (= 3 (test-struct-slot :b obj)))
    
    ;; Test setting values via dispatcher
    (test-struct-slot :b obj :set-value 10)
    (is (= 10 (test-struct-b obj)))
    
    ;; Test the generated -find predicates by passing a list of structs
    (is-true  (test-struct-a-find (list (make-test-struct :a "init") 
                                        (make-test-struct :a "other")) 
                                  obj))
                                  
    ;; False because obj's 'b' slot was updated to 10 above
    (is-false (test-struct-b-find (list (make-test-struct :b 3) 
                                        (make-test-struct :b 4)) 
                                  obj)) 
                                  
    ;; True because 10 is in this new list of structs
    (is-true  (test-struct-b-find (list (make-test-struct :b 9) 
                                        (make-test-struct :b 10) 
                                        (make-test-struct :b 11)) 
                                  obj))))
