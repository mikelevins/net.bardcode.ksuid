;;;; ***********************************************************************
;;;;
;;;; Name:          test.lisp
;;;; Project:       ksuid
;;;; Purpose:       tests for the ksuid implementation
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :net.bardcode.ksuid-test)

;;; ---------------------------------------------------------------------
;;; test suite definition
;;; ---------------------------------------------------------------------

(def-suite ksuid-tests
  :description "Tests for the KSUID implementation")

(in-suite ksuid-tests)

(defun run-tests ()
  "Run all KSUID tests and return T if all pass."
  (run! 'ksuid-tests))

;;; ---------------------------------------------------------------------
;;; basic creation and type tests
;;; ---------------------------------------------------------------------

(test make-ksuid-creates-valid-type
  "make-ksuid should create a valid KSUID byte array"
  (let ((k (ksuid:make-ksuid)))
    (is (ksuid:ksuid-p k))
    (is (= (length k) ksuid:+ksuid-byte-length+))
    (is (typep k '(array (unsigned-byte 8) (20))))))

(test make-ksuid-with-timestamp
  "make-ksuid should accept an optional timestamp"
  (let* ((ts 12345678)
         (k (ksuid:make-ksuid ts)))
    (is (ksuid:ksuid-p k))
    (is (= (ksuid:ksuid-timestamp k) ts))))

(test make-ksuid-from-parts
  "make-ksuid-from-parts should construct KSUID from timestamp and payload"
  (let* ((ts 12345678)
         (payload (make-array 16 :element-type '(unsigned-byte 8)
                                 :initial-contents '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
         (k (ksuid:make-ksuid-from-parts ts payload)))
    (is (ksuid:ksuid-p k))
    (is (= (ksuid:ksuid-timestamp k) ts))
    (is (equalp (ksuid:ksuid-payload k) payload))))

(test make-ksuid-from-parts-rejects-wrong-payload-size
  "make-ksuid-from-parts should reject payloads that aren't 16 bytes"
  (signals error
    (ksuid:make-ksuid-from-parts 12345 (make-array 10 :element-type '(unsigned-byte 8)))))

;;; ---------------------------------------------------------------------
;;; string conversion tests
;;; ---------------------------------------------------------------------

(test ksuid-string-length
  "ksuid->string should always return exactly 27 characters"
  (dotimes (i 10)
    (let* ((k (ksuid:make-ksuid))
           (s (ksuid:ksuid->string k)))
      (is (= (length s) ksuid:+ksuid-string-length+)))))

(test ksuid-string-padding
  "ksuid->string should left-pad with zeros for small values"
  ;; Create a KSUID with all zeros (smallest possible)
  (let* ((s (ksuid:ksuid->string ksuid:+ksuid-nil+)))
    (is (= (length s) 27))
    (is (string= s "000000000000000000000000000"))))

(test ksuid-string-max
  "ksuid->string should handle max KSUID correctly"
  (let* ((s (ksuid:ksuid->string ksuid:+ksuid-max+)))
    (is (= (length s) 27))
    (is (string= s ksuid:+ksuid-max-string-encoded+))))

(test ksuid-string-roundtrip
  "Converting KSUID to string and back should preserve the value"
  (dotimes (i 10)
    (let* ((k1 (ksuid:make-ksuid))
           (s (ksuid:ksuid->string k1))
           (k2 (ksuid:string->ksuid s)))
      (is (equalp k1 k2)))))

(test ksuid-string-characters
  "ksuid->string should only use base62 characters"
  (dotimes (i 10)
    (let* ((k (ksuid:make-ksuid))
           (s (ksuid:ksuid->string k)))
      (is (every (lambda (ch) (find ch ksuid:+base62-alphabet+)) s)))))

;;; ---------------------------------------------------------------------
;;; integer conversion tests
;;; ---------------------------------------------------------------------

(test ksuid-integer-roundtrip
  "Converting KSUID to integer and back should preserve the value"
  (dotimes (i 10)
    (let* ((k1 (ksuid:make-ksuid))
           (n (ksuid:ksuid->integer k1))
           (k2 (ksuid:integer->ksuid n)))
      (is (equalp k1 k2)))))

(test ksuid-nil-integer
  "Nil KSUID should convert to integer 0"
  (is (= (ksuid:ksuid->integer ksuid:+ksuid-nil+) 0)))

(test ksuid-max-integer
  "Max KSUID should convert to correct maximum integer"
  (let ((expected (1- (ash 1 160))))  ; 2^160 - 1
    (is (= (ksuid:ksuid->integer ksuid:+ksuid-max+) expected))))

;;; ---------------------------------------------------------------------
;;; accessor tests
;;; ---------------------------------------------------------------------

(test ksuid-timestamp-accessor
  "ksuid-timestamp should extract the correct timestamp"
  (let* ((ts 98765432)
         (k (ksuid:make-ksuid ts)))
    (is (= (ksuid:ksuid-timestamp k) ts))))

(test ksuid-time-accessor
  "ksuid-time should return correct universal time"
  (let* ((ts 12345678)
         (k (ksuid:make-ksuid ts))
         (expected-ut (+ ts ksuid:+ksuid-universal-time-epoch+)))
    (is (= (ksuid:ksuid-time k) expected-ut))))

(test ksuid-payload-accessor
  "ksuid-payload should return the correct 16-byte payload"
  (let* ((payload (make-array 16 :element-type '(unsigned-byte 8)
                                 :initial-contents '(255 254 253 252 251 250 249 248
                                                     247 246 245 244 243 242 241 240)))
         (k (ksuid:make-ksuid-from-parts 0 payload))
         (extracted (ksuid:ksuid-payload k)))
    (is (= (length extracted) 16))
    (is (equalp extracted payload))))

;;; ---------------------------------------------------------------------
;;; predicate tests
;;; ---------------------------------------------------------------------

(test ksuid-p-predicate
  "ksuid-p should correctly identify KSUIDs"
  (is (ksuid:ksuid-p (ksuid:make-ksuid)))
  (is (ksuid:ksuid-p ksuid:+ksuid-nil+))
  (is (ksuid:ksuid-p ksuid:+ksuid-max+))
  (is (not (ksuid:ksuid-p "not a ksuid")))
  (is (not (ksuid:ksuid-p 12345)))
  (is (not (ksuid:ksuid-p (make-array 10 :element-type '(unsigned-byte 8))))))

(test ksuid-nil-p-predicate
  "ksuid-nil-p should correctly identify nil KSUIDs"
  (is (ksuid:ksuid-nil-p ksuid:+ksuid-nil+))
  (is (not (ksuid:ksuid-nil-p ksuid:+ksuid-max+)))
  (is (not (ksuid:ksuid-nil-p (ksuid:make-ksuid)))))

(test valid-ksuid-string-p-predicate
  "valid-ksuid-string-p should correctly validate KSUID strings"
  ;; Valid strings
  (is (ksuid:valid-ksuid-string-p ksuid:+ksuid-min-string-encoded+))
  (is (ksuid:valid-ksuid-string-p ksuid:+ksuid-max-string-encoded+))
  (is (ksuid:valid-ksuid-string-p (ksuid:ksuid->string (ksuid:make-ksuid))))
  ;; Invalid: wrong length
  (is (not (ksuid:valid-ksuid-string-p "abc")))
  (is (not (ksuid:valid-ksuid-string-p "0000000000000000000000000000")))  ; 28 chars
  (is (not (ksuid:valid-ksuid-string-p "00000000000000000000000000")))   ; 26 chars
  ;; Invalid: bad characters
  (is (not (ksuid:valid-ksuid-string-p "00000000000000000000000000!")))  ; invalid char
  (is (not (ksuid:valid-ksuid-string-p "00000000000000000000000000-")))  ; invalid char
  (is (not (ksuid:valid-ksuid-string-p "000000000000000000000 00000")))  ; space in middle
  ;; Invalid: exceeds max
  (is (not (ksuid:valid-ksuid-string-p "zzzzzzzzzzzzzzzzzzzzzzzzzzz"))))

;;; ---------------------------------------------------------------------
;;; comparison tests
;;; ---------------------------------------------------------------------

(test ksuid-compare-equal
  "ksuid-compare should return 0 for equal KSUIDs"
  (let ((k (ksuid:make-ksuid)))
    (is (= (ksuid:ksuid-compare k k) 0))
    (is (= (ksuid:ksuid-compare ksuid:+ksuid-nil+ ksuid:+ksuid-nil+) 0))
    (is (= (ksuid:ksuid-compare ksuid:+ksuid-max+ ksuid:+ksuid-max+) 0))))

(test ksuid-compare-less-than
  "ksuid-compare should return -1 when first is less"
  (is (= (ksuid:ksuid-compare ksuid:+ksuid-nil+ ksuid:+ksuid-max+) -1))
  (let* ((k1 (ksuid:make-ksuid 1000))
         (k2 (ksuid:make-ksuid 2000)))
    (is (= (ksuid:ksuid-compare k1 k2) -1))))

(test ksuid-compare-greater-than
  "ksuid-compare should return 1 when first is greater"
  (is (= (ksuid:ksuid-compare ksuid:+ksuid-max+ ksuid:+ksuid-nil+) 1))
  (let* ((k1 (ksuid:make-ksuid 2000))
         (k2 (ksuid:make-ksuid 1000)))
    (is (= (ksuid:ksuid-compare k1 k2) 1))))

(test ksuid-comparison-predicates
  "Comparison predicates should work correctly"
  (let* ((k1 (ksuid:make-ksuid 1000))
         (k2 (ksuid:make-ksuid 2000))
         (k1-copy (ksuid:string->ksuid (ksuid:ksuid->string k1))))
    ;; Less than
    (is (ksuid:ksuid< k1 k2))
    (is (not (ksuid:ksuid< k2 k1)))
    (is (not (ksuid:ksuid< k1 k1-copy)))
    ;; Less than or equal
    (is (ksuid:ksuid<= k1 k2))
    (is (ksuid:ksuid<= k1 k1-copy))
    (is (not (ksuid:ksuid<= k2 k1)))
    ;; Equal
    (is (ksuid:ksuid= k1 k1-copy))
    (is (not (ksuid:ksuid= k1 k2)))
    ;; Greater than or equal
    (is (ksuid:ksuid>= k2 k1))
    (is (ksuid:ksuid>= k1 k1-copy))
    (is (not (ksuid:ksuid>= k1 k2)))
    ;; Greater than
    (is (ksuid:ksuid> k2 k1))
    (is (not (ksuid:ksuid> k1 k2)))
    (is (not (ksuid:ksuid> k1 k1-copy)))))

(test ksuid-string-sorting-matches-binary
  "String sorting should match binary sorting (lexicographic property)"
  (let* ((ksuids (loop repeat 20 collect (ksuid:make-ksuid)))
         (strings (mapcar #'ksuid:ksuid->string ksuids))
         (sorted-by-binary (ksuid:ksuid-sort ksuids))
         (sorted-by-string (mapcar #'ksuid:string->ksuid (sort (copy-list strings) #'string<))))
    (is (every #'equalp sorted-by-binary sorted-by-string))))

;;; ---------------------------------------------------------------------
;;; sorting tests
;;; ---------------------------------------------------------------------

(test ksuid-sort-function
  "ksuid-sort should sort KSUIDs in ascending order"
  (let* ((k1 (ksuid:make-ksuid 1000))
         (k2 (ksuid:make-ksuid 2000))
         (k3 (ksuid:make-ksuid 3000))
         (unsorted (list k3 k1 k2))
         (sorted (ksuid:ksuid-sort unsorted)))
    (is (ksuid:ksuid= (first sorted) k1))
    (is (ksuid:ksuid= (second sorted) k2))
    (is (ksuid:ksuid= (third sorted) k3))))

(test ksuid-sort-does-not-modify-original
  "ksuid-sort should not modify the original sequence"
  (let* ((k1 (ksuid:make-ksuid 3000))
         (k2 (ksuid:make-ksuid 1000))
         (original (list k1 k2))
         (sorted (ksuid:ksuid-sort original)))
    (declare (ignore sorted))
    (is (ksuid:ksuid= (first original) k1))
    (is (ksuid:ksuid= (second original) k2))))

(test ksuid-sorted-p-predicate
  "ksuid-sorted-p should correctly identify sorted sequences"
  (let* ((k1 (ksuid:make-ksuid 1000))
         (k2 (ksuid:make-ksuid 2000))
         (k3 (ksuid:make-ksuid 3000)))
    (is (ksuid:ksuid-sorted-p (list k1 k2 k3)))
    (is (ksuid:ksuid-sorted-p (list k1)))
    (is (ksuid:ksuid-sorted-p '()))
    (is (not (ksuid:ksuid-sorted-p (list k3 k1 k2))))))

;;; ---------------------------------------------------------------------
;;; navigation tests (next/prev)
;;; ---------------------------------------------------------------------

(test ksuid-next-increments
  "ksuid-next should increment the KSUID by 1"
  (let* ((k1 (ksuid:make-ksuid))
         (k2 (ksuid:ksuid-next k1))
         (n1 (ksuid:ksuid->integer k1))
         (n2 (ksuid:ksuid->integer k2)))
    (is (= n2 (1+ n1)))
    (is (ksuid:ksuid< k1 k2))))

(test ksuid-prev-decrements
  "ksuid-prev should decrement the KSUID by 1"
  (let* ((k1 (ksuid:make-ksuid))
         (k2 (ksuid:ksuid-prev k1))
         (n1 (ksuid:ksuid->integer k1))
         (n2 (ksuid:ksuid->integer k2)))
    (is (= n2 (1- n1)))
    (is (ksuid:ksuid< k2 k1))))

(test ksuid-next-prev-roundtrip
  "ksuid-next followed by ksuid-prev should return original"
  (let* ((k1 (ksuid:make-ksuid))
         (k2 (ksuid:ksuid-next k1))
         (k3 (ksuid:ksuid-prev k2)))
    (is (equalp k1 k3))))

(test ksuid-prev-next-roundtrip
  "ksuid-prev followed by ksuid-next should return original"
  (let* ((k1 (ksuid:make-ksuid))
         (k2 (ksuid:ksuid-prev k1))
         (k3 (ksuid:ksuid-next k2)))
    (is (equalp k1 k3))))

(test ksuid-next-at-max-returns-nil
  "ksuid-next on max KSUID should return nil"
  (is (null (ksuid:ksuid-next ksuid:+ksuid-max+))))

(test ksuid-prev-at-nil-returns-nil
  "ksuid-prev on nil KSUID should return nil"
  (is (null (ksuid:ksuid-prev ksuid:+ksuid-nil+))))

(test ksuid-next-handles-byte-overflow
  "ksuid-next should correctly handle byte overflow"
  ;; Create a KSUID where the last byte is 255
  (let* ((payload (make-array 16 :element-type '(unsigned-byte 8) :initial-element 255))
         (k1 (ksuid:make-ksuid-from-parts 0 payload))
         (k2 (ksuid:ksuid-next k1)))
    (is (ksuid:ksuid-p k2))
    (is (= (ksuid:ksuid->integer k2) (1+ (ksuid:ksuid->integer k1))))))

;;; ---------------------------------------------------------------------
;;; reference implementation compatibility tests
;;; ---------------------------------------------------------------------

(test known-ksuid-string-parsing
  "Should correctly parse known KSUID strings from reference implementation"
  ;; Test vector from Go reference: 0ujtsYcgvSTl8PAuAdqWYSMnLOv
  ;; Raw: 0669F7EFB5A1CD34B5F99D1154FB6853345C9735
  ;; Timestamp: 107608047
  ;; Payload: B5A1CD34B5F99D1154FB6853345C9735
  (let* ((test-string "0ujtsYcgvSTl8PAuAdqWYSMnLOv")
         (k (ksuid:string->ksuid test-string))
         (roundtrip (ksuid:ksuid->string k)))
    (is (string= test-string roundtrip))
    (is (= (ksuid:ksuid-timestamp k) 107608047))))

(test timestamp-ordering
  "KSUIDs created with later timestamps should sort after earlier ones"
  (let* ((earlier (ksuid:make-ksuid 1000000))
         (later (ksuid:make-ksuid 2000000)))
    (is (ksuid:ksuid< earlier later))
    (is (string< (ksuid:ksuid->string earlier) (ksuid:ksuid->string later)))))

;;; ---------------------------------------------------------------------
;;; edge case tests
;;; ---------------------------------------------------------------------

(test zero-timestamp-ksuid
  "Should handle KSUID with zero timestamp"
  (let* ((payload (make-array 16 :element-type '(unsigned-byte 8) :initial-element 42))
         (k (ksuid:make-ksuid-from-parts 0 payload)))
    (is (= (ksuid:ksuid-timestamp k) 0))
    (is (= (length (ksuid:ksuid->string k)) 27))))

(test max-timestamp-ksuid
  "Should handle KSUID with maximum 32-bit timestamp"
  (let* ((max-ts (1- (ash 1 32)))  ; 2^32 - 1
         (payload (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))
         (k (ksuid:make-ksuid-from-parts max-ts payload)))
    (is (= (ksuid:ksuid-timestamp k) max-ts))
    (is (= (length (ksuid:ksuid->string k)) 27))))

(test uniqueness
  "Multiple KSUIDs created in sequence should all be unique"
  (let ((ksuids (loop repeat 100 collect (ksuid:make-ksuid))))
    (is (= (length ksuids)
           (length (remove-duplicates ksuids :test #'equalp))))))

;;; ---------------------------------------------------------------------
;;; end of file
;;; ---------------------------------------------------------------------
