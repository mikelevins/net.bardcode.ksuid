;;;; ***********************************************************************
;;;;
;;;; Name:          net.bardcode.ksuid.asd
;;;; Project:       ksuid
;;;; Purpose:       naturally-ordered, collision-free, coordination-free
;;;;                unique identifiers
;;;;                (see https://github.com/segmentio/ksuid)
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem "net.bardcode.ksuid"
  :description "A Common Lisp implementation of KSUID (K-Sortable Unique Identifiers)"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :depends-on (:bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "ksuid"))
  :in-order-to ((asdf:test-op (asdf:test-op "net.bardcode.ksuid/test"))))

(asdf:defsystem "net.bardcode.ksuid/test"
  :description "Tests for net.bardcode.ksuid"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :depends-on (:net.bardcode.ksuid :fiveam)
  :serial t
  :components ((:file "test"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call :net.bardcode.ksuid-test :run-tests)))

;;; The runtime system depends only on bordeaux-threads (used to
;;; serialize access to the shared random state). Tests are isolated
;;; in net.bardcode.ksuid/test so consumers don't pull in fiveam.

#+repl (asdf:load-system :net.bardcode.ksuid)
#+repl (asdf:test-system :net.bardcode.ksuid)
