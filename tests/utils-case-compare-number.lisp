(in-package :maximilian-utils/tests)

(def-suite case-compare-number-suite
  :description "Test suite for case-compare-number macro.")
(in-suite case-compare-number-suite)

(test exact-number-matching
  "Verify exact equality matching against literal numbers."
  (is (string= "two"
               (case-compare-number 2
                 (1 "one")
                 (2 "two")
                 (3 "three"))))
  (is (string= "three"
               (case-compare-number 3.0
                 (1 "one")
                 (2 "two")
                 (3 "three")))))

(test inequality-and-cons-expressions
  "Verify relational operator expressions (<, >, <=, >=, etc.)."
  (is (eq :less
          (case-compare-number 2
            ((< 3) :less)
            ((> 3) :greater))))
  (is (eq :greater
          (case-compare-number 10
            ((< 5) :less)
            ((> 5) :greater))))
  (is (eq :even
          (case-compare-number 6
            ((evenp) :even)
            (otherwise :odd)))))

(test fallthrough-handling
  "Verify matching of T and OTHERWISE fallback clauses."
  (is (string= "fallback-otherwise"
               (case-compare-number 100
                 (1 "one")
                 ((< 10) "small")
                 (otherwise "fallback-otherwise"))))
  (is (string= "fallback-t"
               (case-compare-number 100
                 (1 "one")
                 ((< 10) "small")
                 (t "fallback-t")))))

(test multiple-forms-per-clause
  "Verify that clauses containing multiple body forms execute and return the last value."
  (let ((side-effect nil))
    (is (eq :done
            (case-compare-number 4
              ((< 2) :skip)
              ((> 3)
               (setf side-effect :executed)
               :done))))
    (is (eq :executed side-effect))))

(test single-evaluation-of-keyform
  "Verify that the number-form expression is evaluated exactly once."
  (let ((eval-count 0))
    (flet ((get-number ()
             (incf eval-count)
             5))
      (is (eq :matched
              (case-compare-number (get-number)
                (1 :no)
                ((< 3) :no)
                (5 :matched)
                (otherwise :fallback))))
      (is (= 1 eval-count)))))

(test variable-reference-in-test
  "Verify that bound lexical variables in check positions are evaluated."
  (let ((target 42))
    (is (eq :hit
            (case-compare-number 42
              (10 :miss)
              (target :hit)
              (otherwise :miss))))))

(test no-match-returns-nil
  "Verify that falling through without a matching clause returns NIL (standard COND behavior)."
  (is (null (case-compare-number 42
              (1 :one)
              (2 :two)
              ((< 0) :negative)))))

(test or-number 
  "Verifying \"or\" works"
  (is (eq :matched
          (case-compare-number 4
            (6 :no)
            ((or 1 2 3 4 5) :matched)
            (t :no)))))
