

#| 
tests created with ai.... yeah i hate ai but for this specific purpose i find
little benefit to writing my own tests and also saves a lot of time
|#

(defpackage :maximilian-utils/tests
  (:use :cl :maximilian-utils :fiveam))

(in-package :maximilian-utils/tests)

;; Define the main test suite
(def-suite :maximilian-utils-suite
  :description "Main test suite for the maximilian-utils package")

(in-suite :maximilian-utils-suite)

;; -----------------------------------------------------------------------------
;; String Utilities
;; -----------------------------------------------------------------------------

(test string-splitting
  (is (equal '("a" "b" "c") (split "," "a,b,c")))
  (is (equal '("a" "b" "c") (split-by-char "a,b,c" :split-char #\,)))
  (is (equal '("a" "b,c")   (split "," "a,b,c" :max-count 2))))

(test string-counting-and-subseq
  (is (= 2 (substr-count "hello world hello" "hello")))
  (is (= 0 (substr-count "hello world" "bye")))
  
  (is (equal "def" (subseq-after "abc-def" #\- :exclude-first t)))
  (is (equal "-def" (subseq-after "abc-def" #\- :exclude-first nil)))
  ;; Testing the from-end logic used in `get-file-type`
  (is (equal "txt" (subseq-after "archive.tar.txt" #\. :from-end t :exclude-first t))))

(test string-conversion
  (is (eq :TXT (get-file-type "document.txt")))
  (is (eq :HELLO (string-to-keyword "hello")))
  (is (eq :|hello| (string-to-keyword "hello" :keep-case t)))
  (is (eq '|WORLD| (string-to-symbol "world" :package :maximilian-utils/tests)))
  (is (eq '|world| (string-to-symbol "world" :keep-case t :package :maximilian-utils/tests))))

(test joining
  (is (equal "A-B-C" (join "-" 'a 'b 'c)))
  (is (eq '|A-B-C| (join-symbols "-" 'a 'b 'c))))

;; -----------------------------------------------------------------------------
;; Data Structure Utilities
;; -----------------------------------------------------------------------------

(test alist-and-plist
  (is-true  (alistp '((a . 1) (b . 2))))
  (is-false (alistp '(a b c)))
  (is (equal 1 (assoc-val 'a '((a . 1) (b . 2)))))
  (is (equal '(:a 1 :b 2) (create-plist '(:a :b) '(1 2)))))

(test leaf-operations
  (let ((nested-data '((1 2) #(3 4) 5)))
    (is (= 5 (count-leaves nested-data)))
    (is (equal '(1 2 3 4 5) (get-leaves nested-data)))
    (is (= 15 (reduce-leaves #'+ nested-data)))))

(test hash-table-init
  (let ((ht (make-hash-table)))
    ;; Should initialize and return 42
    (is (= 42 (gethash-init :answer ht 42)))
    ;; Should retain and return 42, ignoring 100
    (is (= 42 (gethash-init :answer ht 100)))
    (is (= 42 (gethash :answer ht)))))

(test circular-lists
  (let ((circ (make-circular (list 1 2 3))))
    (is (= 1 (car circ)))
    (is (= 2 (cadr circ)))
    (is (= 3 (caddr circ)))
    ;; The fourth element should loop back to the first
    (is (= 1 (cadddr circ)))
    ;; Ensure eq identity holds for the circular reference
    (is (eq circ (cdddr circ)))))

;; -----------------------------------------------------------------------------
;; Functional & Macro Utilities
;; -----------------------------------------------------------------------------

(test lambdas-and-booleans
  (is (= 5 (funcall (λ (x) (+ x 2)) 3)))
  (is-true  (bool-val 5))
  (is-true  (bool-val t))
  (is-false (bool-val nil))
  (is-false (return-nil 1 2 3 "anything")))

(test partial-application
  ;; bind
  (is (= 15 (funcall (bind #'+ 5) 10)))
  ;; bind-m
  (is (= 15 (funcall (bind-m + 5) 10)))
  ;; bind-places
  (let ((formatter (bind-places #'format (_ "~A" _))))
    (is (equal "HELLO" (funcall formatter nil "HELLO")))))

(test piping
  ;; (pipe 5 (1+) (* 2)) => (* (1+ 5) 2) = 12 ... wait, let's look at the macro expansion
  ;; The macro nests them: (pipe 5 (1+) (list :a)) => (list :a (1+ 5))
  (is (= 6 (pipe 5 (1+))))
  (is (equal '(:a 6) (pipe 5 (1+) (list :a)))))

;; -----------------------------------------------------------------------------
;; Advanced Macros (defstruct-with-helpers)
;; -----------------------------------------------------------------------------

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
