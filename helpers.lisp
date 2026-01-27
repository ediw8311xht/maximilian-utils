(in-package :my-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstruct-option-parse (name-and-options)
    (if (consp name-and-options)
        (destructuring-bind (name . options) name-and-options
          (values name
                  (loop for (k v) on options
                        when  (and (listp k) (keywordp (car k)))
                        collect k
                        when (keywordp k)
                        collect `(,k ,v))))
        (values name-and-options '()))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slot-name-type (slot-definition)
    (typecase slot-definition
      (atom (values slot-definition nil))
      (list (let ((plist
                    (or (and (keywordp (second slot-definition)) (rest slot-definition))
                        (cddr slot-definition)))) ; when slot contains default value
              (values (first slot-definition) (getf plist :type)))))))

(defmacro defstruct-with-helpers (name-and-options &body body)
  "Creates structure with function structname-slot-find for each slot.

  structname-slot-find: takes input list and struct returning tail of list of first matching
  element on slot

  Optionally takes :export argument to automatically export functions created
  by defstruct and this macro"


  (multiple-value-bind (name options) (defstruct-option-parse name-and-options)
    (let* ((find-defuns         '()) ; symbols created by this macro to be exported
           (symbols-to-export   '()) ; symbols automatically created by defstruct to be exported
           (cname               (or (second (assoc :conc-name options))
                                    (format nil "~A-" name)))
           (to-export           (second (assoc :export options)))
           (predicate           (assoc :predicate options))
           (predicate-val       (second predicate))
           (constructor         (assoc :constructor options))
           (constructor-val     (second constructor))
           (n-options           (remove :export options :key #'car))
           (n-name-and-options  (cons name n-options))
           (docstring           (when (stringp (car body)) (car body))); ignored for now, might have add parsing for this later
           (slots               (if docstring (cdr body) body)))
      ;; adding constructor and predicate to export list (symbols-to-export)
      (when to-export
        (push name symbols-to-export)
        (cond
          ((and constructor constructor-val) (push constructor-val symbols-to-export))
          ((not constructor) (push (intern (format nil "MAKE-~A" name)) symbols-to-export))
          (t "(:constructor nil) tells defstruct not to define constructor"))
        (cond
          ((and predicate predicate-val) (push predicate-val symbols-to-export))
          ((not predicate) (push (intern (format nil "~A-P" name)) symbols-to-export))
          (t "(:predicate nil) tells defstruct not to define predicate")))
      ;; create helper functions
      (dolist (slot slots)
        (multiple-value-bind (slot-name type) (slot-name-type slot)
          (let ((find-funcname (intern (format nil "~A~A-FIND" cname slot-name)))
                (func-accessor (intern (format nil "~A~A" cname slot-name))))
            (push
              `(defun ,find-funcname (input-list bookmark)
                 (member (,func-accessor bookmark) input-list :test #'equalp :key #',func-accessor))
              find-defuns)
            (when to-export
              (push find-funcname symbols-to-export)
              (push func-accessor symbols-to-export)))))
      ;; insert code
      `(progn
         (defstruct ,n-name-and-options ,@body)
         ,@(reverse find-defuns)
         ,(when to-export `(export ',(reverse symbols-to-export)))))))

(defmacro gethash-init (key hash-table &body set-form
                        &aux (e-key   (gensym))
                        (e-hash-table (gensym))
                        (e-value      (gensym))
                        (e-found      (gensym)))
  "Gets value at key in hash-table and sets it to value of `set-form` if it
  doesn't already exist."
  `(let ((,e-key ,key) (,e-hash-table ,hash-table))
     (multiple-value-bind (,e-value ,e-found)
       (gethash ,e-key ,e-hash-table)
       (if ,e-found
           ,e-value
           (setf (gethash ,e-key ,e-hash-table)
                 (progn ,@set-form))))))

(defun split-by-char (str &key (split-char #\,))
  (loop for c across (format nil "~a~c" str split-char)
        for i from 0
        with s = 0
        when (char= c split-char)
        collect (subseq str s i)
        and do (setf s (+ 1 i))))

(defun substr-count (str sub &optional (len (length sub)) (pos (- (length str) len)))
  (if (> 0 pos)
      0
      (+ (substr-count str sub len (- pos 1))
         (if (string-equal sub (subseq str pos (+ len pos)))
             1
             0))))

(defun format-combine (&optional s &rest rest)
  (if (not s)
      ""
      (loop with arg with rest-args = rest
            repeat (substr-count s "~A")
            do (setf (values arg rest-args)
                     (apply #'format-combine rest-args))
            collect arg into args
            finally (return (values (apply #'format nil s args) rest-args)))))

(defun assoc-val (symbol assoc-list)
  (let ((key-val (assoc symbol assoc-list)))
    (cond
      ((consp key-val)  (cdr key-val))
      (key-val          key-val))))

(defun show-structure (var &key (level 1) (max-level 5) (indent-size 2))
  (format t "~VT~S~%" (* level indent-size) (type-of var))

  (let ((level (+ 1 level)))
    (unless (< max-level level)
      (typecase var
        (hash-table
          (maphash (lambda (key val)
                     (declare (ignore key))
                     (show-structure val :level level))
                   var))
        (list
          (fresh-line)
          (loop for i in var
                do (show-structure i :level level)))
        (t nil)))))


(defun join (sep &rest rest)
  (with-output-to-string (output)
    (format output "~A" (car rest))
    (loop for i in (cdr rest)
          do (format output "~A~A" sep i))))

(defun join-symbols (sep &rest rest)
  (intern (apply #'join sep rest)))

(defun return-nil (&rest rest)
  (declare (ignore rest)) nil)

(defun alistp (alist)
  (if alist
      (and (consp (first alist))
           (alistp (rest alist)))
      t))

(defun subseq-after (str character &key (foundp nil) (from-end nil) (exclude-first nil))
  (let* ((pos (position character str :from-end from-end))
         (pos (if (and pos exclude-first) (+ pos 1) pos)))
    (if pos (subseq str pos) foundp)))

(defun reduce-leaves (func input-data &key (key #'identity))
  "Reduce but for atoms in data structure and nested data structures."
  (labels
    ((reduce-main (data)
       (typecase data
         (null
           (funcall key nil))
         (string
           (funcall key data))
         (vector
           (reduce func data :key #'reduce-main))
         (cons
           (funcall func (reduce-main (car data))
                    (reduce-main (cdr data))))
         (hash-table
           (loop for value being the hash-values of data
                 for acc = (funcall func (reduce-main value))
                 then      (funcall func acc (reduce-main value))
                 finally (return acc)))
         (t (funcall key data)))))
    (reduce-main input-data)))

(defun get-leaves (input-data)
  "Returns list of atoms in data structure and nested data structures."
  (reduce-leaves #'append input-data :key #'(lambda (x) (when x (list x)))))

(defun count-leaves (input-data)
  "Returns numbers of atoms in data structure and nested data structures."
  (reduce-leaves #'+ input-data :key #'(lambda (x) (if x 1 0))))

(defun get-file-type (input-file)
  (intern
    (string-upcase (my-utils:subseq-after input-file #\. :from-end t :exclude-first 1))
    "KEYWORD"))

