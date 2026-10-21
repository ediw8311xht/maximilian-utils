
(in-package :maximilian-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun make-val-method (method-name class form slot-names)
    (let ((g-object-arg (gensym "OBJECT")))
      (list
        `(defgeneric ,method-name (object))
        `(defmethod  ,method-name ((,g-object-arg ,class))
           (with-slots ,slot-names ,g-object-arg
             ,form)))))
  (defun make-val-compare-method (method-name val-fn class fn)
    (let ((g-object-arg1 (gensym "OBJECT-1"))
          (g-object-arg2 (gensym "OBJECT-2")))
      (list
        `(defgeneric ,method-name (object1 object2))
        `(defmethod  ,method-name ((,g-object-arg1 ,class) (,g-object-arg2 ,class))
           (,fn (,val-fn ,g-object-arg1) (,val-fn ,g-object-arg2))))))
  (defun extract-slot-name (slot)
    (if (consp slot)
        (car slot)
        slot))

  )

(defmacro defclass-with-helpers ((name &key export
                                       with-value
                                       (value-name 'value)
                                       (conc-name name))
                                 (&rest super-classes) (&rest slots) &body options)

    (let* ((fn-list   '()) ; functions to define
           (to-export '()) ; symbols to export
           (package (or (symbol-package name) *package*))
           (slot-names (mapcar #'extract-slot-name slots))
           (val-method-name  (join-symbols-package "-" package conc-name value-name))
           )
      (when export (push name to-export))
      (when with-value
        (mapc #'(lambda (x) (push x fn-list))
              (make-val-method val-method-name name with-value slot-names))
        (when export (push val-method-name to-export))

        (loop for fn in '(= < > <= >= /=)
              do (let ((method-name (join-symbols-package "" package conc-name fn)))
                   (mapc #'(lambda (x) (push x fn-list))
                         (make-val-compare-method method-name val-method-name name fn))
                   (when export (push method-name to-export)))))
      `(progn
         (defclass ,name (,@super-classes) (,@slots) ,@options)
         ,@(reverse fn-list)
         ,(when (and export to-export)
            ;`(export ',(reverse to-export))
            `(export ',(reverse to-export)))

         )))
