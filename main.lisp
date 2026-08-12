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
        if (eq i '>>)
        do  (setf results (list (append (reverse current) results)))
        (setf current nil)
        else
        do (push i current)
        finally (return (append (reverse current) results))))

(defmacro bind-m (func &rest bind-args)
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
        if (eq x sep) collect y          into unbound
        else          collect (list y x) into bound
        collect y into complete-args
        finally (return
                  `(let (,@bound (,f ,func))
                     (lambda (,@unbound &rest rest)
                       (apply ,f ,@complete-args rest))))))

(defun bind (func &rest bind-args)
  (lambda (&rest rest-args)
    (apply func (append bind-args rest-args))))

(defun split (split-str str &key (max-count nil) &aux (s (length split-str)))
  (labels
    ((split-rec (str max-count)
       (let ((i (search split-str str))
             (max-count (when max-count (- 1 max-count))))
         (cond
           ((not i) (list str))
           ((and max-count (< max-count 1))
            (cons (subseq str 0 i)
                  (list (subseq str (+ s i)))))
           (t (cons (subseq str 0 i)
                    (split-rec (subseq str (+ s i)) max-count)))))))
    (split-rec str max-count)))

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
  (if s
      (loop with arg with rest-args = rest
            repeat (substr-count s "~A")
            do (setf (values arg rest-args)
                     (apply #'format-combine rest-args))
            collect arg into args
            finally (return (values (apply #'format nil s args) rest-args)))
      ""))

(defun assoc-val (symbol assoc-list)
  (cdr (assoc symbol assoc-list)))

(defun show-structure (var &key (level 1)
                           (max-level 5)
                           (indent-size 2)
                           (output-func (lambda (var) (type-of var)))
                           (output-stream *STANDARD-OUTPUT*))
  (format output-stream "~VT~@{~A~}~%" (* level indent-size) (funcall output-func var))

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


(defun join (sep &rest rest)
  (format nil (format nil "~~{~~A~~^~A~~}" sep) rest))

(defun join-symbols (sep &rest rest)
  (intern (apply #'join sep rest)))

(defun return-nil (&rest rest)
  (declare (ignore rest)) nil)

(defun alistp (alist)
  (if alist
      (and (consp (first alist))
           (alistp (rest alist)))
      t))

(defun subseq-after (str character
                         &key (foundp nil)
                         (from-end nil)
                         (exclude-first nil))
  (let ((pos (position character str :from-end from-end)))
    (if pos (subseq str (if exclude-first (+ pos 1) pos))
        foundp)))

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

(defun get-file-type (input-file)
  (intern
    (string-upcase (subseq-after input-file #\. :from-end t :exclude-first 1))
    :keyword))

(defun string-to-keyword (s &key keep-case)
  (intern (if keep-case s (string-upcase s)) 
          :keyword))

(defun string-to-symbol (s &key keep-case)
  (intern (if keep-case s (string-upcase s))))

(defun create-plist (props &optional vals)
  (loop for x in props
        for y = (when vals (pop vals))
        collect x collect y))


(defun string-to-pathname (str &optional (start 0) (end (length str)))
  (parse-namestring
    (with-output-to-string (output)
      (labels ((varcharp (c) (or (alphanumericp c) (char= c #\_)))
               (handle-var (p)
                 (let ((next (position-if-not #'varcharp str :start p :end end)))
                   (format output "~A" (or (uiop:getenv (subseq str p next)) ""))
                   (or next end)))
               (rec-h (p)
                 (let ((next (position #\$ str :start p :end end :test #'char=)))
                   (format output "~A" (subseq str p next))
                   (when next
                     (rec-h (handle-var (+ 1 next))))))
               (handle-first ()
                 (if (char= (aref str start) #\~)
                     (progn (format output "~A" (or (uiop:getenv "HOME") ""))
                            (rec-h (+ 1 start)))
                     (rec-h start))))
        (handle-first)))))

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

(defun timestamp-to-ntp (s &optional (epoch :unix))
  (case epoch
    (:unix (- s 2208988800))
    (t     s)))

(defun utc-format (s &key (epoch :ntp) utc stream)
  (multiple-value-call #'format stream s
    (if utc (decode-universal-time 
              (timestamp-to-ntp s epoch)) 
        (get-decoded-time))))

(defun utc-alist (&optional utc)
  (mapcar #'cons '(:second :minute :hour :day :month :year :day-of-week :daylight-savings :timezone)
          (multiple-value-list (if utc (decode-universal-time utc) 
                                   (get-decoded-time)))))

