
(in-package :maximilian-utils/tests)

(def-suite memoize-suite 
  :description "Test suite for defun-memoize macro."
  :in utils-suite)
(in-suite memoize-suite)

(test factorial-execution
  "Verify mathematical correctness and recursion handling."
  (defun-memoize test-factorial (n)
    (if (<= n 1)
        1
        (* n (test-factorial (- n 1)))))
  (is (= 120 (test-factorial 5)))
  (is (= 720 (test-factorial 6))))

(test memoization-hit-count
  "Verify that the body only executes once per unique argument list."
  (let ((call-count 0))
    (defun-memoize expensive-op (x y)
      (incf call-count)
      (+ x y))
    
    (is (= 0 call-count))
    (is (= 7 (expensive-op 3 4)))
    (is (= 1 call-count))
    
    ;; Subsequent call with identical args must hit cache
    (is (= 7 (expensive-op 3 4)))
    (is (= 1 call-count))
    
    ;; Call with distinct args must compute and increment count
    (is (= 9 (expensive-op 4 5)))
    (is (= 2 call-count))))

(test nil-value-caching
  "Verify that returning NIL is distinguished from a cache miss."
  (let ((call-count 0))
    (defun-memoize returns-nil (x)
      (incf call-count)
      (if (evenp x) nil x))
    
    (is (null (returns-nil 2)))
    (is (= 1 call-count))
    
    ;; Calling again must return NIL from cache without evaluating body
    (is (null (returns-nil 2)))
    (is (= 1 call-count))))

(test optional-argument-handling
  "Verify destructuring behavior with optional arguments."
  (defun-memoize opt-fn (a &optional (b 10))
    (+ a b))
  (is (= 15 (opt-fn 5)))
  (is (= 25 (opt-fn 5 20))))
