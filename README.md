# net.bardcode.ksuid

A Common Lisp implementation of [KSUID](https://github.com/segmentio/ksuid) (K-Sortable Unique Identifiers).

## Overview

KSUID is a 20-byte globally unique identifier that is naturally sortable by generation time:

- **Bytes 0-3**: 32-bit big-endian UTC timestamp (custom epoch: May 13, 2014)
- **Bytes 4-19**: 128-bit random payload

The text representation is always 27 characters using base62 encoding (alphanumeric, no delimiters).

## Installation

```lisp
(ql:quickload :net.bardcode.ksuid)
```

**Dependencies**: `fiveam` (for tests only)

This implementation has **no external dependencies** for KSUID generation itself. Random bytes are generated using the standard Common Lisp `RANDOM` function with a state initialized from system entropy.

**Note**: The random number generator is NOT cryptographically secure, but is sufficient for generating unique identifiers in most applications. 

## Usage

### Creating KSUIDs

```lisp
;; Create a new KSUID with current timestamp
(ksuid:make-ksuid)
;; => #(14 23 156 42 ...)  ; 20-byte array

;; Create with specific timestamp (seconds since KSUID epoch)
(ksuid:make-ksuid 12345678)

;; Create from parts (timestamp + 16-byte payload)
(ksuid:make-ksuid-from-parts 12345678 payload-bytes)
```

### String Conversion

```lisp
;; Convert to 27-character base62 string
(ksuid:ksuid->string (ksuid:make-ksuid))
;; => "2NxGkHJIzV0Uj5p9CvXYbOsW3A1"

;; Parse from string
(ksuid:string->ksuid "2NxGkHJIzV0Uj5p9CvXYbOsW3A1")
;; => #(14 23 156 42 ...)
```

### Accessing Components

```lisp
(let ((k (ksuid:make-ksuid)))
  ;; Get raw timestamp (seconds since KSUID epoch)
  (ksuid:ksuid-timestamp k)
  ;; => 347123456
  
  ;; Get timestamp as Lisp universal time
  (ksuid:ksuid-time k)
  ;; => 3956112256
  
  ;; Get the 16-byte random payload
  (ksuid:ksuid-payload k)
  ;; => #(182 45 231 ...)
)
```

### Comparison and Sorting

```lisp
(let ((k1 (ksuid:make-ksuid))
      (k2 (ksuid:make-ksuid)))
  ;; Compare (-1, 0, or 1)
  (ksuid:ksuid-compare k1 k2)
  
  ;; Comparison predicates
  (ksuid:ksuid< k1 k2)
  (ksuid:ksuid<= k1 k2)
  (ksuid:ksuid= k1 k2)
  (ksuid:ksuid>= k1 k2)
  (ksuid:ksuid> k1 k2)
  
  ;; Sort a list of KSUIDs
  (ksuid:ksuid-sort (list k2 k1))
  
  ;; Check if sorted
  (ksuid:ksuid-sorted-p (list k1 k2)))
```

### Navigation

```lisp
(let ((k (ksuid:make-ksuid)))
  ;; Get next KSUID (increment by 1)
  (ksuid:ksuid-next k)
  
  ;; Get previous KSUID (decrement by 1)
  (ksuid:ksuid-prev k))
```

### Predicates

```lisp
;; Type check
(ksuid:ksuid-p thing)

;; Check for nil KSUID (all zeros)
(ksuid:ksuid-nil-p k)

;; Validate string format
(ksuid:valid-ksuid-string-p "2NxGkHJIzV0Uj5p9CvXYbOsW3A1")
;; => T
```

### Constants

```lisp
ksuid:+ksuid-nil+              ; All-zero KSUID
ksuid:+ksuid-max+              ; Maximum KSUID (all 255s)
ksuid:+ksuid-min-string-encoded+ ; "000000000000000000000000000"
ksuid:+ksuid-max-string-encoded+ ; "aWgEPTl1tmebfsQzFP4bxwgy80V"
ksuid:+ksuid-byte-length+      ; 20
ksuid:+ksuid-string-length+    ; 27
ksuid:+ksuid-timestamp-length+ ; 4
ksuid:+ksuid-payload-length+   ; 16
```

## Running Tests

```lisp
(asdf:load-system :net.bardcode.ksuid)
(net.bardcode.ksuid-test:run-tests)
```

## Compatibility with Reference Implementation

This implementation is compatible with the [reference Go implementation](https://github.com/segmentio/ksuid):

- Same binary format (20 bytes: 4-byte BE timestamp + 16-byte payload)
- Same epoch (May 13, 2014)
- Same base62 alphabet (`0-9A-Za-z`)
- Same string length (always 27 characters)
- Lexicographic sorting works identically

## Comparison with Other Versions

| Feature | ksuid (v0.1.x) | ksuid2 (v0.2.x) | ksuid3 (v0.3.x) |
|---------|----------------|-----------------|-----------------|
| Random source | CL random | Ironclad | CL random |
| Crypto-secure | No | Yes | No |
| Dependencies | cl-intbytes | ironclad | None* |
| String padding | No | Yes | Yes |
| Accessors | No | Yes | Yes |
| Comparison | No | Yes | Yes |
| Navigation | No | Yes | Yes |

\* fiveam is only required for running tests

## License

Apache 2.0

## Author

mikel evins <mikel@evins.net>
