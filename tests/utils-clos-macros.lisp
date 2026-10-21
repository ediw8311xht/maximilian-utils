
(in-package :maximilian-utils/tests)

(def-suite clos-macros-suite
  :description "clos-macros testing"
  :in utils-suite)
(in-suite clos-macros-suite)

(defclass-with-helpers
  (my-class :with-value (+ a b)
            :export nil) ()
  ((a :initarg :a :initform 0 :type integer)
   b))

(test value-comparing
  (let ((c1 (make-instance 'my-class :a 10))
        (c2 (make-instance 'my-class :a 11)))
    (setf (slot-value c1 'b) 12)
    (setf (slot-value c2 'b) 10)
    (is (= 22 (my-class-value c1) ))
    (is (= 21 (my-class-value c2) ))
    (loop for (fn out) in '((my-class= nil) (my-class< nil) (my-class<= nil) (my-class> t) (my-class>= t) (my-class/= t))
          do (is (equal out (funcall fn c1 c2))))))


