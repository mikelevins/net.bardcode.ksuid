;;;; ***********************************************************************
;;;;
;;;; Name:          ksuid.lisp
;;;; Project:       ksuid
;;;; Purpose:       naturally-ordered, collision-free, coordination-free
;;;;                unique identifiers.
;;;;                see https://github.com/segmentio/ksuid [MIT license]
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:net.bardcode.ksuid)

;;; ---------------------------------------------------------------------
;;; parameters and constants
;;; ---------------------------------------------------------------------

;;; Size constants
(defconstant +ksuid-byte-length+ 20
  "Total byte length of a KSUID.")

(defconstant +ksuid-timestamp-length+ 4
  "Byte length of the timestamp portion.")

(defconstant +ksuid-payload-length+ 16
  "Byte length of the random payload portion.")

(defconstant +ksuid-string-length+ 27
  "Character length of the base62-encoded string representation.")

;;; Random state for KSUID generation (initialized from system entropy)
(defvar *ksuid-random-state* (make-random-state t)
  "Random state used for generating KSUID payloads.
Initialized from system entropy at load time.")

(defvar *ksuid-random-lock* (bordeaux-threads:make-lock "ksuid-random")
  "Lock protecting concurrent access to *KSUID-RANDOM-STATE*.
Some implementations document RANDOM as thread-safe with a shared
state, but this is not guaranteed across implementations, so we
lock explicitly.")

;;; Base62 encoding alphabet (matches reference implementation)
(defparameter +base62-alphabet+ "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
  "Base62 alphabet used for string encoding. Lexicographically ordered.")

;;; Time epoch constants
(defparameter +unix-universal-time-epoch+ 2208988800
  "Difference between Unix epoch (1970-01-01) and Lisp universal time epoch (1900-01-01).")

;;; The ksuid timestamp counts seconds from the following Lisp
;;; universal time. This universal time corresponds to timestamp May
;;; 13, 2014. A 32-bit timestamp gives ksuids about 136 years of
;;; useful life beyond the epoch.

(defparameter +ksuid-universal-time-epoch+ 3608988800
  "Lisp universal time corresponding to the KSUID epoch (May 13, 2014).")

(defparameter +ksuid-unix-epoch-offset+ (- +ksuid-universal-time-epoch+ +unix-universal-time-epoch+)
  "Offset from Unix epoch to KSUID epoch in seconds.")

(defparameter +ksuid-unix-epoch-seconds+ 1400000000
  "The KSUID epoch (May 13, 2014) expressed as Unix timestamp seconds.
Useful for cross-language interop where the other side computes
timestamps from the Unix epoch -- e.g., a JavaScript counterpart
computing a KSUID timestamp as (Math.floor(Date.now() / 1000) -
KSUID_UNIX_EPOCH_SECONDS).")

;;; Nil and Max KSUIDs (matches reference implementation)
;;;
;;; These arrays are intended to be treated as immutable; callers
;;; must not mutate them. Use COPY-SEQ if a mutable copy is needed.

(defparameter +ksuid-nil+ (make-array +ksuid-byte-length+
                                       :element-type '(unsigned-byte 8)
                                       :initial-element 0)
  "Represents a completely empty (invalid) KSUID. Treat as immutable.")

(defparameter +ksuid-max+ (make-array +ksuid-byte-length+
                                       :element-type '(unsigned-byte 8)
                                       :initial-element 255)
  "Represents the highest value a KSUID can have. Treat as immutable.")

;;; String bounds for validation (matches reference implementation)
(defparameter +ksuid-min-string-encoded+ "000000000000000000000000000"
  "Minimum valid base62-encoded KSUID string.")

(defparameter +ksuid-max-string-encoded+ "aWgEPTl1tmebfsQzFP4bxwgy80V"
  "Maximum valid base62-encoded KSUID string.")

;;; ---------------------------------------------------------------------
;;; type definition
;;; ---------------------------------------------------------------------

(deftype ksuid ()
  "A KSUID is a 20-byte array: 4 bytes timestamp (big-endian) + 16 bytes random payload."
  '(array (unsigned-byte 8) (20)))

(defun ksuid-p (thing)
  "Returns T if THING is a valid KSUID byte array."
  (typep thing 'ksuid))

;;; ---------------------------------------------------------------------
;;; internal helpers
;;; ---------------------------------------------------------------------

(defun ksuid-time-now ()
  "Returns the current time as seconds since the KSUID epoch."
  (- (get-universal-time) +ksuid-universal-time-epoch+))

(defun get-ksuid-timestamp-bytes (&optional time-seconds)
  "Returns a 4-byte array representing TIME-SECONDS in big-endian format.
If TIME-SECONDS is nil, uses the current KSUID time."
  (let ((ts (or time-seconds (ksuid-time-now)))
        (bytes (make-array +ksuid-timestamp-length+ :element-type '(unsigned-byte 8))))
    ;; Big-endian encoding: most significant byte first
    (setf (aref bytes 0) (ldb (byte 8 24) ts))
    (setf (aref bytes 1) (ldb (byte 8 16) ts))
    (setf (aref bytes 2) (ldb (byte 8 8) ts))
    (setf (aref bytes 3) (ldb (byte 8 0) ts))
    bytes))

(defun get-ksuid-random-bytes ()
  "Returns a 16-byte array of random bytes.
Uses the standard Common Lisp random number generator with a state
initialized from system entropy, serialized through *KSUID-RANDOM-LOCK*
for thread safety. Note: This is NOT cryptographically secure, but is
sufficient for generating unique identifiers."
  (let ((bytes (make-array +ksuid-payload-length+ :element-type '(unsigned-byte 8))))
    (bordeaux-threads:with-lock-held (*ksuid-random-lock*)
      (dotimes (i +ksuid-payload-length+)
        (setf (aref bytes i) (random 256 *ksuid-random-state*))))
    bytes))

(defun base-62-digit-value (ch)
  "Returns the numeric value of a base62 digit character."
  (position ch +base62-alphabet+ :test 'char=))

;;; ---------------------------------------------------------------------
;;; constructors
;;; ---------------------------------------------------------------------

(defun make-ksuid (&optional timestamp-seconds)
  "Creates a new KSUID with the current time (or TIMESTAMP-SECONDS if provided)
and a random payload."
  (let ((result (make-array +ksuid-byte-length+ :element-type '(unsigned-byte 8)))
        (ts-bytes (get-ksuid-timestamp-bytes timestamp-seconds))
        (rand-bytes (get-ksuid-random-bytes)))
    ;; Copy timestamp bytes (0-3)
    (dotimes (i +ksuid-timestamp-length+)
      (setf (aref result i) (aref ts-bytes i)))
    ;; Copy random payload bytes (4-19)
    (dotimes (i +ksuid-payload-length+)
      (setf (aref result (+ i +ksuid-timestamp-length+)) (aref rand-bytes i)))
    result))

(defun make-ksuid-from-parts (timestamp-seconds payload)
  "Creates a KSUID from a timestamp (seconds since KSUID epoch) and a 16-byte payload.
PAYLOAD must be a sequence of exactly 16 bytes."
  (assert (= (length payload) +ksuid-payload-length+) ()
          "Payload must be exactly ~D bytes, got ~D" +ksuid-payload-length+ (length payload))
  (let ((result (make-array +ksuid-byte-length+ :element-type '(unsigned-byte 8)))
        (ts-bytes (get-ksuid-timestamp-bytes timestamp-seconds)))
    ;; Copy timestamp bytes (0-3)
    (dotimes (i +ksuid-timestamp-length+)
      (setf (aref result i) (aref ts-bytes i)))
    ;; Copy payload bytes (4-19)
    (dotimes (i +ksuid-payload-length+)
      (setf (aref result (+ i +ksuid-timestamp-length+))
            (if (arrayp payload)
                (aref payload i)
                (elt payload i))))
    result))

;;; ---------------------------------------------------------------------
;;; predicates
;;; ---------------------------------------------------------------------

(defun ksuid-nil-p (ksuid)
  "Returns T if KSUID is the nil KSUID (all zeros)."
  (and (ksuid-p ksuid)
       (every #'zerop ksuid)))

(defun valid-ksuid-string-p (thing)
  "Returns T if THING is a valid base62-encoded KSUID string.
Checks length, character set, and value bounds."
  (and (stringp thing)
       (= (length thing) +ksuid-string-length+)
       (every (lambda (ch) (find ch +base62-alphabet+)) thing)
       ;; Check bounds: string must be >= min and <= max
       (string>= thing +ksuid-min-string-encoded+)
       (string<= thing +ksuid-max-string-encoded+)))

;;; ---------------------------------------------------------------------
;;; accessors
;;; ---------------------------------------------------------------------

(defun ksuid-timestamp (ksuid)
  "Returns the raw 32-bit timestamp from a KSUID (seconds since KSUID epoch).
This is the uncorrected timestamp value."
  (assert (ksuid-p ksuid) () "Not a ksuid: ~S" ksuid)
  ;; Big-endian decode of first 4 bytes
  (+ (ash (aref ksuid 0) 24)
     (ash (aref ksuid 1) 16)
     (ash (aref ksuid 2) 8)
     (aref ksuid 3)))

(defun ksuid-time (ksuid)
  "Returns the timestamp from a KSUID as a Lisp universal time."
  (+ (ksuid-timestamp ksuid) +ksuid-universal-time-epoch+))

(defun ksuid-payload (ksuid)
  "Returns a copy of the 16-byte random payload from a KSUID."
  (assert (ksuid-p ksuid) () "Not a ksuid: ~S" ksuid)
  (let ((payload (make-array +ksuid-payload-length+ :element-type '(unsigned-byte 8))))
    (dotimes (i +ksuid-payload-length+)
      (setf (aref payload i) (aref ksuid (+ i +ksuid-timestamp-length+))))
    payload))

;;; ---------------------------------------------------------------------
;;; conversions
;;; ---------------------------------------------------------------------

(defun ksuid->integer (ksuid)
  "Converts a KSUID byte array to a 160-bit integer."
  (assert (ksuid-p ksuid) () "Not a ksuid: ~S" ksuid)
  (let ((result 0))
    ;; Big-endian: first byte is most significant
    (dotimes (i +ksuid-byte-length+)
      (setf result (+ (ash result 8) (aref ksuid i))))
    result))

(defun integer->ksuid (int)
  "Converts a 160-bit integer to a KSUID byte array."
  (assert (and (integerp int) (>= int 0)) () "Invalid integer for KSUID: ~S" int)
  (let ((result (make-array +ksuid-byte-length+ :element-type '(unsigned-byte 8))))
    ;; Big-endian: fill from end to start
    (loop for i from (1- +ksuid-byte-length+) downto 0
          do (setf (aref result i) (ldb (byte 8 0) int)
                   int (ash int -8)))
    result))

(defun ksuid->string (ksuid)
  "Converts a KSUID to its 27-character base62 string representation.
The output is always exactly 27 characters, left-padded with '0' if necessary."
  (assert (ksuid-p ksuid) () "Not a ksuid: ~S" ksuid)
  (let* ((num (ksuid->integer ksuid))
         (digits '()))
    ;; Convert to base62 digits (least significant first)
    (if (zerop num)
        (setf digits (list #\0))
        (loop while (> num 0)
              do (multiple-value-bind (q rem) (truncate num 62)
                   (push (char +base62-alphabet+ rem) digits)
                   (setf num q))))
    ;; Build string and left-pad to 27 characters
    (let ((base62 (coerce digits 'string)))
      (if (< (length base62) +ksuid-string-length+)
          (concatenate 'string
                       (make-string (- +ksuid-string-length+ (length base62))
                                    :initial-element #\0)
                       base62)
          base62))))

(defun string->ksuid-int (str)
  "Converts a base62 KSUID string to its integer representation."
  (assert (valid-ksuid-string-p str) () "Not a valid ksuid string: ~S" str)
  (let ((int-value 0))
    ;; Process left to right (most significant first)
    (loop for ch across str
          for digit-val = (base-62-digit-value ch)
          do (setf int-value (+ (* int-value 62) digit-val)))
    int-value))

(defun string->ksuid (str)
  "Converts a base62 KSUID string to a KSUID byte array."
  (assert (valid-ksuid-string-p str) () "Not a valid ksuid string: ~S" str)
  (integer->ksuid (string->ksuid-int str)))

;;; ---------------------------------------------------------------------
;;; comparison and sorting
;;; ---------------------------------------------------------------------

(defun ksuid-compare (a b)
  "Compares two KSUIDs lexicographically.
Returns -1 if A < B, 0 if A = B, 1 if A > B."
  (assert (and (ksuid-p a) (ksuid-p b)) () "Arguments must be KSUIDs")
  (dotimes (i +ksuid-byte-length+ 0)
    (let ((byte-a (aref a i))
          (byte-b (aref b i)))
      (cond ((< byte-a byte-b) (return -1))
            ((> byte-a byte-b) (return 1))))))

(defun ksuid< (a b)
  "Returns T if KSUID A is less than KSUID B."
  (= (ksuid-compare a b) -1))

(defun ksuid<= (a b)
  "Returns T if KSUID A is less than or equal to KSUID B."
  (<= (ksuid-compare a b) 0))

(defun ksuid= (a b)
  "Returns T if KSUID A equals KSUID B."
  (= (ksuid-compare a b) 0))

(defun ksuid>= (a b)
  "Returns T if KSUID A is greater than or equal to KSUID B."
  (>= (ksuid-compare a b) 0))

(defun ksuid> (a b)
  "Returns T if KSUID A is greater than KSUID B."
  (= (ksuid-compare a b) 1))

(defun ksuid-sort (ksuids)
  "Returns a sorted copy of the list/vector of KSUIDs (ascending order)."
  (sort (copy-seq ksuids) #'ksuid<))

(defun ksuid-sorted-p (ksuids)
  "Returns T if the sequence of KSUIDs is sorted in ascending order."
  (let ((len (length ksuids)))
    (or (< len 2)
        (loop for i from 0 below (1- len)
              always (ksuid<= (elt ksuids i) (elt ksuids (1+ i)))))))

;;; ---------------------------------------------------------------------
;;; navigation (next/prev)
;;; ---------------------------------------------------------------------

(defun ksuid-next (ksuid)
  "Returns the next KSUID after KSUID (incrementing by 1).
Returns nil if KSUID is already at maximum."
  (assert (ksuid-p ksuid) () "Not a ksuid: ~S" ksuid)
  (if (equalp ksuid +ksuid-max+)
      nil
      (let ((result (make-array +ksuid-byte-length+ :element-type '(unsigned-byte 8)))
            (carry 1))
        ;; Copy and increment from least significant byte
        (loop for i from (1- +ksuid-byte-length+) downto 0
              do (let ((sum (+ (aref ksuid i) carry)))
                   (setf (aref result i) (logand sum #xFF))
                   (setf carry (ash sum -8))))
        result)))

(defun ksuid-prev (ksuid)
  "Returns the previous KSUID before KSUID (decrementing by 1).
Returns nil if KSUID is already at minimum (nil KSUID)."
  (assert (ksuid-p ksuid) () "Not a ksuid: ~S" ksuid)
  (if (equalp ksuid +ksuid-nil+)
      nil
      (let ((result (make-array +ksuid-byte-length+ :element-type '(unsigned-byte 8)))
            (borrow 1))
        ;; Copy and decrement from least significant byte
        (loop for i from (1- +ksuid-byte-length+) downto 0
              do (let ((diff (- (aref ksuid i) borrow)))
                   (if (< diff 0)
                       (setf (aref result i) (+ diff 256)
                             borrow 1)
                       (setf (aref result i) diff
                             borrow 0))))
        result)))
