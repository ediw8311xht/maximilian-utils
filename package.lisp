
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
    #:count-leaves
    #:create-plist
    #:defstruct-with-helpers
    #:format-combine
    #:get-file-type
    #:get-leaves
    #:gethash-init
    #:join
    #:join-symbols
    #:pipe
    #:pipe-arrow
    #:reduce-leaves
    #:return-nil
    #:show-structure
    #:split
    #:split-by-char
    #:string-to-keyword
    #:subseq-after
    #:substr-count
    #:string-to-pathname
    ))

