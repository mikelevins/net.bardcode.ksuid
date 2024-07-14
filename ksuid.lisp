;;;; ***********************************************************************
;;;;
;;;; Name:          ksuid.lisp
;;;; Project:       ksuid
;;;; Purpose:       naturally-ordered, collision-free,coordination-free,
;;;;                dependency-free unique identifiers.
;;;;                see https://github.com/segmentio/ksuid [MIT license]
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.ksuid)

;;; ---------------------------------------------------------------------
;;; parameters
;;; ---------------------------------------------------------------------

(defvar +128-set-bits+ #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)
(defvar +base62-alphabet+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
(defparameter +unix-universal-time-epoch+ 2208988800)

;;; *ksuid-universal-time-epoch*

;;; the ksuid timestamp counts seconds from the following Lisp
;;; universal time. This universal time corresponds to timestamp
;;; @2014-05-13T11:53:20.000000-05:00. A 32-bit timestamp gives
;;; ksuids about 136 years of useful life beyond the epoch.
(defparameter +ksuid-universal-time-epoch+ 3608988800)
(defparameter +ksuid-unix-epoch-offset+ (- +ksuid-universal-time-epoch+ +unix-universal-time-epoch+))


;;; ---------------------------------------------------------------------
;;; computing ksuids
;;; ---------------------------------------------------------------------

(defun ksuid-time-now ()
  (- (get-universal-time) +ksuid-universal-time-epoch+))

#+nil (ksuid-time-now)


(defun get-ksuid-timestamp-bytes (&optional time-seconds)
  (reverse (cl-intbytes:int->octets (or time-seconds (ksuid-time-now))
                                    4)))

#+nil (get-ksuid-timestamp-bytes)

(defun get-ksuid-random-bytes (&optional (prng ironclad:*prng*))
  (cl-intbytes:int->octets (ironclad:strong-random +128-set-bits+ prng)
                           16))

#+nil (type-of (get-ksuid-random-bytes))

(deftype ksuid ()
  '(simple-array (unsigned-byte 8) (20)))

(defun make-ksuid ()
  (concatenate '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (20))
               (get-ksuid-timestamp-bytes)
               (get-ksuid-random-bytes)))

#+nil (typep (make-ksuid) 'ksuid)


;;; ---------------------------------------------------------------------
;;; ksuid conversions
;;; ---------------------------------------------------------------------

(defun ksuid->integer (ksuid)
  (cl-intbytes:octets->uint ksuid 20))

#+nil (ksuid->integer (make-ksuid))

(defun integer->ksuid (int)
  (cl-intbytes:int->octets int 20))

#+nil (integer->ksuid 0)
#+nil (defparameter $k (make-ksuid))
#+nil (ksuid->integer $k)
#+nil (integer->ksuid (ksuid->integer $k))
#+nil (equalp $k (integer->ksuid (ksuid->integer $k)))


(defun ksuid->string (ksuid)
  (assert (typep ksuid 'ksuid)() "Not a ksuid: ~S" ksuid)
  (let* ((num (ksuid->integer ksuid))
         (base62 ""))
    (loop for i = num
          while (> num 0)
          do (multiple-value-bind (q rem)(truncate num 62)
               (setf base62 (concatenate 'string
                                         (string (elt +base62-alphabet+ rem))
                                         base62)
                     num q)))
    base62))

#+nil (defparameter $k (make-ksuid))
#+nil (length (ksuid->string $k))

(defmethod valid-ksuid-string-p (thing) nil)
(defmethod valid-ksuid-string-p ((thing string))
  (and (<= (length thing) 27)
       (every (lambda (ch1)(find-if (lambda (ch2)(char= ch1 ch2)) +base62-alphabet+))
              thing)))

#+nil (defparameter $k (make-ksuid))
#+nil (time (valid-ksuid-string-p +base62-alphabet+))
#+nil (valid-ksuid-string-p "+-()")
#+nil (loop for i from 0 below 1000
            do (if (valid-ksuid-string-p (ksuid->string (make-ksuid)))
                   (format t ".")
                   (break)))

(defmethod base-62-digit-value ((ch character))
  (position ch +base62-alphabet+ :test 'char=))

#+nil (time (base-62-digit-value #\W))

(defun string->ksuid (str)
  (assert (valid-ksuid-string-p str)() "Not a valid ksuid string: ~S" str)
  (let* ((len (length str))
         (place-expts (reverse (loop for i from 0 below len collect i)))
         (digit-values (loop for i from 0 below len collect (base-62-digit-value (elt str i))))
         (ksuid-int (loop for i from 0 below len
                          summing (* (elt digit-values i)
                                     (expt 62 (elt place-expts i))))))
    (integer->ksuid ksuid-int)))

#+nil (defparameter $k1 (make-ksuid))
#+nil (setf $kstr (ksuid->string $k1))
#+nil (setf $k2 (string->ksuid $kstr))
#+nil (equalp $k1 $k2)
