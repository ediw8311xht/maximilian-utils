
(in-package :cl-user)
(defpackage :maximilian-utils
  (:use :cl)
  (:nicknames :max-utils)
  (:export
    #|----------macros-------------|#
    #:λ
    #:gethash-init
    #:bind-m
    #:bind-places
    #:pipe
    #:pipe-arrow
    #:case-compare-number
    #:multf
    #:powf
    ;; memoize
    #:defun-memoize
    ;; struct macros
    #:defstruct-with-helpers
    ;; clos macros
    #:defclass-with-helpers

    #|----------functions----------|#
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
    #:join-symbols-package
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
    ;;predicates
    #:alistp
    ;; time functions
    #:timestamp-to-ntp
    #:utc-alist
    #:utc-format
    ))

