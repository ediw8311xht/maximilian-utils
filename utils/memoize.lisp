(in-package :maximilian-utils)


(defmacro defun-memoize (name args &body body)
  (let ((g-htable       (gensym))
        (g-table-got    (gensym))
        (g-table-value  (gensym))
        (g-rest         (gensym))
        ;(g-body-value   (gensym))
        )
    `(let ((,g-htable (make-hash-table :test #'equal)))
       (defun ,(join-symbols "-" 'clear name) () (clrhash ,g-htable))
       (defun ,name (&rest ,g-rest)
         (multiple-value-bind
           (,g-table-value ,g-table-got) (gethash ,g-rest ,g-htable)
           (if ,g-table-got
               ,g-table-value
               (destructuring-bind ,args ,g-rest
                 (setf (gethash ,g-rest ,g-htable) (progn ,@body)))))))))
