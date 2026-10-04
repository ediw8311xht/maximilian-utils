
(in-package :maximilian-utils/tests)
;; Define the test suite
(def-suite split-by-chars-suite
  :description "Test suite for the split-by-chars function.")

(in-suite split-by-chars-suite)

;; 1. Test basic splitting behavior
(test basic-splitting
  "Tests standard splitting with one or multiple characters."
  (is (equal '("hello") (split-by-chars "hello" nil))
      "Passing nil for chars should return a list with the original string")
  (is (equal '("a" "b" "c") (split-by-chars "a,b,c" '(#\,)))
      "Should split by a single character")
  (is (equal '("a" "b" "c") (split-by-chars "a,b;c" '(#\, #\;)))
      "Should split by any character in the provided list"))

;; 2. Test edge cases (empty strings, consecutive delimiters, etc.)
(test edge-cases
  "Tests empty strings, trailing/leading delimiters, and consecutive delimiters."
  (is (equal '("") (split-by-chars "" '(#\,)))
      "Empty string should return a list with an empty string")
  (is (equal '("a" "" "b") (split-by-chars "a,,b" '(#\,)))
      "Consecutive delimiters should produce an empty string in the middle")
  (is (equal '("" "a" "b" "") (split-by-chars ",a,b," '(#\,)))
      "Leading and trailing delimiters should produce empty strings at the ends")
  (is (equal '("" "" "") (split-by-chars ",," '(#\,)))
      "Only delimiters should result in a list of empty strings"))

;; 3. Test the :sharedp functionality (displaced arrays)
(test sharedp-displacement
  "Tests that :sharedp creates displaced arrays pointing to the original string."
  (let* ((orig-string "x,y,z")
         (result (split-by-chars orig-string '(#\,) :sharedp t)))
    
    (is (equal '("x" "y" "z") result)
        "The string content should match standard evaluation")
    
    ;; Verify that the results are actually displaced arrays sharing structure
    ;; with the original string, rather than newly allocated strings.
    (is (eq orig-string (nth-value 0 (array-displacement (first result))))
        "First element should be displaced to orig-string")
    (is (eq orig-string (nth-value 0 (array-displacement (second result))))
        "Second element should be displaced to orig-string")
    (is (eq orig-string (nth-value 0 (array-displacement (third result))))
        "Third element should be displaced to orig-string")
    
    ;; Verify the offsets are correct
    (is (= 0 (nth-value 1 (array-displacement (first result)))))
    (is (= 2 (nth-value 1 (array-displacement (second result)))))
    (is (= 4 (nth-value 1 (array-displacement (third result)))))))
