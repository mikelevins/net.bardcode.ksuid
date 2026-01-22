;;;; ***********************************************************************
;;;;
;;;; Name:          net.bardcode.ksuid.asd
;;;; Project:       ksuid
;;;; Purpose:       naturally-ordered, collision-free, coordination-free,
;;;;                dependency-free unique identifiers
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
  :depends-on (:fiveam)
  :serial t
  :components ((:file "package")
               (:file "ksuid")
               (:file "test")))

;;; Note: This implementation has NO external dependencies for KSUID generation.
;;; The only dependency (fiveam) is for running tests.
;;; 
;;; Random bytes are generated using the standard Common Lisp RANDOM function
;;; with a random state initialized from system entropy via (make-random-state t).
;;; This is NOT cryptographically secure, but is sufficient for generating
;;; unique identifiers in most applications.
;;;
;;; If you need cryptographically secure random bytes, consider using
;;; net.bardcode.ksuid2 which depends on Ironclad.

#+repl (asdf:load-system :net.bardcode.ksuid)
#+repl (net.bardcode.ksuid-test:run-tests)
