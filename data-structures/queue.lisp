
(in-package :maximilian-utils.queue)

#|
heavily inspired by:
https://ssojet.com/data-structures/implement-queue-in-common-lisp
|#

(defstruct (queue
             (:constructor make-queue
              (&key (head 0)
                    (tail 0)
                    (data (make-array 0 :fill-pointer 0 :adjustable t)))))
  (data      #() :type vector)
  (head      0   :type integer)
  (tail      0   :type integer))

(defmethod is-empty ((q queue))
  (>= (queue-head q) (queue-tail q)))

(defmethod dequeue ((q queue))
  (if (is-empty q)
      (error "attempting to dequeue from empty queue")
      (let* ((head   (queue-head q) )
             (data   (queue-data q) )
             (popped (aref data head) ))
        (setf (aref data head) nil)
        (incf (queue-head q))
        popped)))

(defmethod enqueue ((q queue) element)
  (let ((data   (queue-data q) ))
    (vector-push-extend element data)
    (incf (queue-tail q))))


