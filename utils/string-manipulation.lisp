
(in-package :maximilian-utils)

#| strings, chars, and output |#

;; note - going to modify lambda list when opts is not a list/cons/keyword (excluding nil) to be the output stream
(defun easy-pprint (opts &rest rest)
  "pprint with but print all arguments with optional postfix, prefix, and separator"
  (destructuring-bind
    (&key (output *standard-output*)
          (prefix    nil)
          (postfix   #\Newline)
          (separator #\Newline))
    opts
    (format output
            (format nil "~@[~A~]~~{~~A~~^~A~~}~@[~A~]" prefix separator postfix)
            rest)))

(defun join (sep &rest rest)
  (if rest
      (with-output-to-string (s)
        (princ (car rest) s)
        (loop for i in (cdr rest)
              do (princ sep s)
              (princ i s)))
      ""))

(defun join-symbols (sep &rest rest)
  (when rest
    (intern (apply #'join sep rest))))

(defun join-symbols-package (sep package &rest rest)
  (when rest
    (intern (with-output-to-string (s)
              (princ (string (car rest)) s)
              (loop for i in (cdr rest)
                    do (princ sep s)
                    (princ (string i) s)))
            package)))

(defun print-2d-array (array &key
                             (column-separator #\Space)
                             (row-separator    #\Newline)
                             (output-stream *standard-output*)
                             (beginning #\Newline)
                             (end #\Newline))
  "print contents of 2d array"
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

(defun repeat-string (str times &key (out nil))
  "repeat aesthetic representation a certain number times"
  (format out "~v@{~A~:*~}" times str))

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
             collect (subseq str start-index end-index)))))
 
(defun string-to-keyword (s &key keep-case)
  (intern (if keep-case s (string-upcase s))
          :keyword))

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

(defun string-to-symbol (s &key keep-case package)
  (funcall #'intern
           (if keep-case s (string-upcase s))
           package))


(defun substr-count (str sub &optional (len (length sub)) (pos (- (length str) len)))
  (if (> 0 pos)
      0
      (+ (substr-count str sub len (- pos 1))
         (if (string-equal sub (subseq str pos (+ len pos)))
             1
             0))))

(defun subseq-after (str character &key foundp from-end exclude-first sharedp)
  (let ((pos (position character str :from-end from-end)))
    (if pos
        (let ((start (if exclude-first (+ pos 1) pos)))
          (if sharedp
              (make-array (- (length str) start)
                          :element-type 'character
                          :displaced-to str
                          :displaced-index-offset start)
              (subseq str start)))
        foundp)))
