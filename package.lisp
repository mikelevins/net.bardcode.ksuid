;;;; package.lisp

(defpackage :net.bardcode.ksuid
  (:use #:cl)
  (:nicknames :ksuid)
  (:export
   ;; constants
   #:+128-set-bits+
   #:+base62-alphabet+
   #:+ksuid-universal-time-epoch+
   #:+ksuid-unix-epoch-offset+
   #:+unix-universal-time-epoch+
   #:+ksuid-string-length+
   #:+ksuid-byte-length+
   #:+ksuid-timestamp-length+
   #:+ksuid-payload-length+
   #:+ksuid-nil+
   #:+ksuid-max+
   #:+ksuid-min-string-encoded+
   #:+ksuid-max-string-encoded+
   ;; type
   #:ksuid
   ;; constructors
   #:make-ksuid
   #:make-ksuid-from-parts
   ;; predicates
   #:ksuid-p
   #:ksuid-nil-p
   #:valid-ksuid-string-p
   ;; accessors
   #:ksuid-timestamp
   #:ksuid-time
   #:ksuid-payload
   ;; conversions
   #:integer->ksuid
   #:ksuid->integer
   #:ksuid->string
   #:string->ksuid
   #:string->ksuid-int
   ;; comparison and sorting
   #:ksuid-compare
   #:ksuid<
   #:ksuid<=
   #:ksuid=
   #:ksuid>=
   #:ksuid>
   #:ksuid-sort
   #:ksuid-sorted-p
   ;; navigation
   #:ksuid-next
   #:ksuid-prev
   ))


(defpackage :net.bardcode.ksuid-test
  (:use #:cl #:it.bese.fiveam)
  (:local-nicknames (#:ksuid #:net.bardcode.ksuid)
                    (#:fiveam #:it.bese.fiveam))
  (:export #:run-tests))
