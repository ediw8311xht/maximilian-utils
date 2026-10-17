
(in-package :maximilian-utils)

(defun assoc-val (symbol assoc-list)
  (cdr (assoc symbol assoc-list)))

(defun bind (func &rest bind-args)
  (lambda (&rest rest-args)
    (apply func (append bind-args rest-args))))

(defun bool-val (v) (not (not v)))

(defun directory-recursive-files (path fn &key (max-depth nil))
  "Call function `fn` on all files within directories and subdirectories of `path`
  Limit depth of directory to recurse into using `max-depth` with 1 being direct directories of path."
  (unless (uiop:directory-exists-p path)
    (error "Directory, '~A', couldn't be found." path))
  (when (and max-depth (or (not (integerp max-depth)) (< max-depth 0)))
    (error ":max-depth must be nil or a non-negative integer. passed value: ~A" max-depth))
  (let ((path-length (- (length (pathname-directory path)) 1))) 
    (uiop:collect-sub*directories
      path
      (constantly t)
      (if max-depth
          (lambda (subdir) (> max-depth (- (length (pathname-directory subdir))
                                           path-length)))
          (constantly t))
      (lambda (subdir)
        (mapc fn (uiop:directory-files subdir))))))

(defun make-circular (l &key (sharedp nil))
  (if sharedp
      (and (setf (cdr (last l)) l) l)
      (let ((n-l (copy-list l)))
        (setf (cdr (last n-l)) n-l))))

(defun return-nil (&rest rest)
  (declare (ignore rest)) nil)

(defun get-file-type (input-file)
  (intern
    (string-upcase (subseq-after input-file #\. :from-end t :exclude-first 1))
    :keyword))

(defun create-plist (props &optional vals)
  (loop for x in props
        for y = (when vals (pop vals))
        collect x collect y))

(defun format-combine (&optional s &rest rest)
  (if s
      (loop with arg with rest-args = rest
            repeat (substr-count s "~A")
            do (setf (values arg rest-args)
                     (apply #'format-combine rest-args))
            collect arg into args
            finally (return (values (apply #'format nil s args) rest-args)))
      ""))

(defun show-structure (var &key (level 1)
                           (max-level 5)
                           (indent-size 2)
                           (output-func (lambda (var) (type-of var)))
                           (output-stream *STANDARD-OUTPUT*))
  (format output-stream "~VT~@{~A~}~%"
          (* level indent-size)
          (if output-func
              (funcall output-func var)
              var))

  (let ((level (+ 1 level)))
    (unless (< max-level level)
      (typecase var
        (hash-table
          (maphash (lambda (key val)
                     (declare (ignore key))
                     (show-structure val :level level :indent-size indent-size :output-func output-func :output-stream output-stream))
                   var))
        (list
          (fresh-line)
          (loop for i in var
                do (show-structure i :level level :indent-size indent-size :output-func output-func :output-stream output-stream)))
        (t nil)))))

(defun reduce-leaves (func input-data
                           &key
                           (key #'identity)
                           (ignore-nil t)
                           (initial-value nil initial-value-p)
                           &aux
                           (acc initial-value)
                           (first-val-p initial-value-p))
  "Reduce but for atoms in data structure and nested data structures."
  (labels
    ((update-value (data-atom)
       (let ((result (funcall key data-atom)))
         (if first-val-p
             (setf acc (funcall func acc result))
             (setf acc result))
         (setf first-val-p t)))
     (reduce-main (data)
       (typecase data
         (null   (unless ignore-nil
                   (update-value nil)))
         (string (update-value data))
         (vector (map nil #'reduce-main data))
         (cons   (mapc #'reduce-main data))
         (hash-table
           (loop for value being the hash-values of data
                 do (reduce-main value)))
         (t (update-value data)))))
    (reduce-main input-data)
    acc))

(defun get-leaves (input-data)
  "Returns list of atoms in data structure and nested data structures."
  (reduce-leaves #'append input-data :key (lambda (x) (when x (list x)))))

(defun count-leaves (input-data)
  "Returns numbers of atoms in data structure and nested data structures."
  (reduce-leaves #'+ input-data :key (lambda (x) (if x 1 0))))

