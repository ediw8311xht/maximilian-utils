(in-package :maximilian-utils)

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

(defun split-by-chars (str chars &key sharedp)
  (cond 
    ((not chars) (list str))
    (t (loop with str-app = (format nil "~A~C" str (car chars))
             for start-index = 0 then (+ end-index 1)
             for end-index = (position-if #'(lambda (c) (find c chars)) str-app :start start-index)
             while end-index
             if sharedp
             collect (make-array (- end-index start-index) :element-type 'character :displaced-to str :displaced-index-offset start-index)
             else
             collect (subseq str start-index end-index)
             ))))

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

(defun string-to-symbol (s &key keep-case package)
  (funcall #'intern 
           (if keep-case s (string-upcase s))
           package))

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
(defun make-circular (l &key (sharedp nil))
  (if sharedp
      (and (setf (cdr (last l)) l) l)
      (let ((n-l (copy-list l)))
        (setf (cdr (last n-l)) n-l))))

(defun print-2d-array (array &key
                             (column-separator #\Space)
                             (row-separator    #\Newline)
                             (output-stream *standard-output*)
                             (end #\Newline)
                             (beginning #\Newline))
  (loop 
    with (cols rows) = (array-dimensions array)
    with start       = (- cols 1)
    initially (when beginning (princ beginning output-stream))
    for y from start downto 0
    when (< y start) do (princ row-separator output-stream)

    do (loop for x from 0 below rows
             when (> x 0) do (princ column-separator output-stream)
             do (princ (aref array y x) output-stream))
    finally (when end (princ end output-stream))))

