(in-package :cl-user)

#| ---------- queue ---------- |#
(defpackage :maximilian-utils.queue
  (:use :cl)
  (:import-from :maximilian-utils
                :defstruct-with-helpers)
  (:nicknames :max-utils.queue)
  (:export
    #:make-queue
    #:queue
    #:queue-head
    #:queue-tail
    #:queue-data
    #:enqueue
    #:dequeue
    ))

