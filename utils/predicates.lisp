
(in-package :maximilian-utils)

(defun alistp (alist)
  (if alist
      (and (consp (first alist))
           (alistp (rest alist)))
      t))

