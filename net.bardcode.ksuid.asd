;;;; ***********************************************************************
;;;;
;;;; Name:          ksuid.asd
;;;; Project:       ksuid
;;;; Purpose:       naturally-ordered, collision-free,coordination-free,
;;;;                dependency-free unique identifiers
;;;;                (see https://github.com/segmentio/ksuid)
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(asdf:defsystem "net.bardcode.ksuid"
  :description "A simple implementation of KSUID in Common Lisp"
  :author "mikel evins <mikel@evins.net>"
  :license  "Apache 2.0"
  :version (:read-file-form "version.lisp")
  :serial t
  :depends-on (:cl-intbytes :local-time)
  :components ((:file "package")
               (:file "ksuid")))

#+nil (asdf:load-system :net.bardcode.ksuid)
#+nil (ql:quickload :net.bardcode.ksuid)
