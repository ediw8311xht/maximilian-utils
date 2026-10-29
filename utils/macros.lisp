
(in-package :maximilian-utils)

(defmacro λ (&body body)
  `(lambda ,@body))

(defmacro gethash-init (key hash-table &body set-form
                        &aux (e-key   (gensym))
                        (e-hash-table (gensym))
                        (e-value      (gensym))
                        (e-found      (gensym)))
  "Gets value at key in hash-table and sets it to value of `set-form` if it
  doesn't already exist."
  `(let ((,e-key ,key)
         (,e-hash-table ,hash-table))
     (multiple-value-bind (,e-value ,e-found) (gethash ,e-key ,e-hash-table)
       (if ,e-found
           ,e-value
           (setf (gethash ,e-key ,e-hash-table)
                 (progn ,@set-form))))))

(defmacro pipe (&body function-calls)
"pipe output of one function into another each being a list:
example:
  ```lisp
  (pipe (+ 3 5) (+ 3) (* 2)) ; -> 22
  ```
"
  (loop for x in (cdr function-calls)
        with return-function = (car function-calls)
        do (setf return-function (append x (list return-function)))
        finally (return return-function)))

(defmacro pipe-arrow (&body body)
  "pipe output of one function into another separated by >>"
  (loop for i in body
        with results = nil
        with current = nil
        if (and (symbolp i) (string= (symbol-name i) ">>"))
        do  (setf results (list (append (reverse current) results)))
        (setf current nil)
        else
        do (push i current)
        finally (return (append (reverse current) results))))

(defmacro bind-m (func &rest bind-args)
  "bind but as macro"
  `(lambda (&rest rest-args)
     (apply #',func ,@bind-args rest-args)))

(defmacro bind-places (func args &key (sep '_)
                       &aux (f (gensym)))
  "Partially apply function setting arguments to specific places.
  Arguments matching &sep (default _) are to be recieved when returned function is called.

  Example: (let ((a (bind-places #'format (_ \"~A\" _))))
             (funcall a nil \"HI\")) ; ->  \"HI\""
  (loop for x in args
        for y = (gensym)
        if (and (symbolp x) (string= (symbol-name x) (symbol-name sep))) collect y into unbound
        else collect (list y x) into bound
        collect y into complete-args
        finally (return
                  `(let (,@bound (,f ,func))
                     (lambda (,@unbound &rest rest)
                       (apply ,f ,@complete-args rest))))))

(defmacro case-compare-number (number-form &body body)
"`case` allowing number comparison
example:
  ```
  (case-compare 4
    (2          \"equal to 2\")
    ((< 3)      \"less than three\")
    (4          \"equal to four\")
    ((> 3)      \"more than three\")
    (otherwise  \"equal to three\"))
  ; \"equal to four\"
  ```"
  (let ((g-number-form (gensym)))
    `(let ((,g-number-form ,number-form))
       (cond
         ,@(loop for (check . form) in body
                 collect (cond
                           ; eval with function
                           ((consp check)
                            (if (eq (car check) 'cl:or)
                               `((member ,g-number-form (list ,@(cdr check))) ,@form)
                               `((,(car check)  ,g-number-form ,@(cdr check)) ,@form)))
                           ; t otherwise handling
                           ((member check '(t otherwise))
                            `(t ,@form))
                           ; var/number handling
                           (t `((= ,check ,g-number-form) ,@form))))))))


(defmacro multf (place number)
  `(setf ,place (* ,place ,number)))

(defmacro powf (place power)
  `(setf ,place (expt ,place ,power)))

;(defmacro deconstruction-bind-special )
