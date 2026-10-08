(in-package :cl-user)
(defpackage :maximilian-utils
  (:use :cl)
  (:nicknames :max-utils)
  (:export
    #:λ
    #:alistp
    #:assoc-val
    #:bind
    #:bind-m
    #:bind-places
    #:bool-val
    #:count-leaves
    #:create-plist
    #:date-alist
    #:defstruct-with-helpers
    #:directory-recursive-files
    #:format-combine
    #:get-file-type
    #:get-leaves
    #:gethash-init
    #:join
    #:join-symbols
    #:make-circular
    #:pipe
    #:pipe-arrow
    #:print-2d-array
    #:reduce-leaves
    #:return-nil
    #:show-structure
    #:split
    #:split-by-char
    #:split-by-chars
    #:string-to-keyword
    #:string-to-pathname
    #:string-to-symbol
    #:subseq-after
    #:substr-count
    #:timestamp-to-ntp
    #:utc-alist
    #:utc-format
    ))

