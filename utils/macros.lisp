(in-package :maximilian-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun defstruct-option-parse (name-and-options)
    (if (consp name-and-options)
        (destructuring-bind (name . options) name-and-options
          (values name (loop for (k v) on options
                             when  (and (listp k) (keywordp (car k)))
                             collect k
                             when (keywordp k)
                             collect `(,k ,v))))
        (values name-and-options '())))

  (defun slot-name-type (slot-definition)
    (typecase slot-definition
      (atom (values slot-definition nil))
      (list (let ((plist (or (and (keywordp (second slot-definition))
                                  (rest slot-definition))
                             (cddr slot-definition)))) ; when slot contains default value
              (values (first slot-definition) (getf plist :type))))))
  (defun remove-options (options to-remove)
    (delete-if (lambda (opt) (find (car opt) to-remove))
               options))
  )

(defmacro defstruct-with-helpers (name-and-options &body body)
  "Creates structure with function structname-slot-find for each slot.

  structname-slot-find: takes input list and struct returning tail of list of first matching element on slot

  Optional arguments (pass as key value pair same as options for defstruct)
  :export [t/nil] - automatically export functions created by defstruct and this macro
  :with-get-set [VALUE] - creates function with name <NAME|CONC-NAME>-<VALUE> that
  gets/sets value of slot on struct with slot keyword representation of slot-name

  Example:
  (defstruct (my-struct (:with-get-set slot) (:export t))
    (a \"initial\" :type string)
    (b 3           :type number))

  (my-struct-slot :a (make-my-struct) ) ; \"initial\"
  (my-struct-slot :b (make-my-struct) :set-value 9) ; #S(MY-STRUCT :A \"initial\" :B 9)
  "


  (multiple-value-bind (name options) (defstruct-option-parse name-and-options)
    (let* ((fn-list             '()) ; functions created by this macro
           (symbols-to-export   '()) ; symbols to export
           (conc-name           (or (second (assoc :conc-name options))
                                    (format nil "~A-" name)))
           (with-get-set        (second (assoc :with-get-set options)))
           (with-get-set-symbol (when with-get-set (intern (format nil "~A~A" conc-name with-get-set))))
           (to-export           (second (assoc :export options)))
           (predicate           (assoc :predicate options))
           (predicate-val       (second predicate))
           (constructor         (assoc :constructor options))
           (constructor-val     (second constructor))
           (n-options           (remove-options options '(:with-get-set :export))) ; options for defstruct (keys for this macro removed)
           (n-name-and-options  (cons name n-options))
           (docstring           (when (stringp (car body)) (car body))) ; ignored for now, might have add parsing for this later
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
      ; create helper functions
      (dolist (slot slots)
        (multiple-value-bind (slot-name type) (slot-name-type slot)
          (declare (ignore type))
          (let ((find-funcname (intern (format nil "~A~A-FIND" conc-name slot-name)))
                (func-accessor (intern (format nil "~A~A" conc-name slot-name))))
            (push
              `(defun ,find-funcname (input-list struct)
                 (member (,func-accessor struct) input-list :test #'equalp :key #',func-accessor))
              fn-list)
            (when with-get-set
              (let ((fn-keyword (intern (symbol-name slot-name) :keyword)))
                (push `(defmethod  ,with-get-set-symbol ((slot (eql ,fn-keyword)) obj &key set-value)
                         (if set-value
                             (setf (,func-accessor obj) set-value)
                             (,func-accessor obj)))
                      fn-list)))
            (when to-export
              (push find-funcname symbols-to-export)
              (push func-accessor symbols-to-export)))))
      ;; insert code
      `(progn
         (defstruct ,n-name-and-options ,@body)
         ,(when with-get-set
            `(defgeneric ,with-get-set-symbol (slot obj &key set-value)))
         ,@(reverse fn-list)
         ,(when to-export `(export ',(reverse symbols-to-export)))
         ',name))))

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
  (loop for x in (cdr function-calls)
        with return-function = (car function-calls)
        do (setf return-function (append x (list return-function)))
        finally (return return-function)))

(defmacro pipe-arrow (&body body)
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
