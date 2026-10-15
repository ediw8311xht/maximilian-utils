(in-package :cl-user)
(defpackage :maximilian-utils
  (:use :cl)
  (:nicknames :max-utils)
  (:export
    ;; macros
    #:λ
    #:defstruct-with-helpers
    #:gethash-init
    #:bind-m
    #:bind-places
    #:defun-memoize
    #:pipe
    #:pipe-arrow
    #:case-compare-number

    ;; functions
    #:alistp
    #:assoc-val
    #:bind
    #:bool-val
    #:count-leaves
    #:create-plist
    #:date-alist
    #:directory-recursive-files
    #:easy-pprint ; todo - add test
    #:format-combine
    #:get-file-type
    #:get-leaves
    #:join
    #:join-symbols
    #:make-circular
    #:print-2d-array
    #:reduce-leaves
    #:repeat-string ; todo - add test
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

