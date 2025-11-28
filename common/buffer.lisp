;;; Binary Buffer Building Utilities for Habu
;;;
;;; These functions build byte buffers in memory as lists of fixnums (0-255).
;;; At the end, convert to vector, then string, then write with native-write-file.
;;;
;;; This replaces stream-based I/O for generating Mach-O executables.
;;; Pure Habu - no SBCL dependencies (no loop, no format, etc.)

(in-package :habu)

;;; ============================================================
;;; Buffer Building
;;; ============================================================

(defun buf-u8 (val)
  "Create buffer with single byte"
  (cons (logand val #xFF) nil))

(defun buf-u16-le (val)
  "Create buffer with 16-bit little-endian value"
  (cons (logand val #xFF)
        (cons (logand (ash val -8) #xFF)
              nil)))

(defun buf-u32-le (val)
  "Create buffer with 32-bit little-endian value"
  (cons (logand val #xFF)
        (cons (logand (ash val -8) #xFF)
              (cons (logand (ash val -16) #xFF)
                    (cons (logand (ash val -24) #xFF)
                          nil)))))

(defun buf-u64-le (val)
  "Create buffer with 64-bit little-endian value"
  (append (buf-u32-le (logand val #xFFFFFFFF))
          (buf-u32-le (logand (ash val -32) #xFFFFFFFF))))

(defun buf-bytes (bytes)
  "Create buffer from list of bytes"
  bytes)

(defun buf-string-padded (str len)
  "Create buffer with string padded to LEN bytes with zeros"
  (let* ((slen (string-length str)))
    (labels ((collect-chars (idx)
               (if (>= idx (if (< slen len) slen len))
                   nil
                   (cons (string-ref str idx)
                         (collect-chars (+ idx 1)))))
             (collect-zeros (count)
               (if (<= count 0)
                   nil
                   (cons 0 (collect-zeros (- count 1))))))
      (append (collect-chars 0)
              (collect-zeros (- len slen))))))

(defun buf-zeros (count)
  "Create buffer with COUNT zero bytes"
  (if (<= count 0)
      nil
      (cons 0 (buf-zeros (- count 1)))))

(defun buf-append-all (bufs)
  "Append all buffers into single buffer"
  (if (null bufs)
      nil
      (append (car bufs) (buf-append-all (cdr bufs)))))

(defun buf-length (buf)
  "Get length of buffer in bytes"
  (length buf))

(defun buf-to-string (buf)
  "Convert buffer (list of bytes) to string for writing"
  (let* ((len (length buf))
         (vec (make-vector len)))
    ;; Fill vector with bytes
    (labels ((fill-vec (remaining idx)
               (if (null remaining)
                   vec
                   (progn
                     (vector-set vec idx (car remaining))
                     (fill-vec (cdr remaining) (+ idx 1))))))
      (let ((filled (fill-vec buf 0)))
        (make-string-from-vector filled)))))

(defun buf-write-file (path buf)
  "Write buffer to file as binary data"
  (let ((str (buf-to-string buf)))
    (native-write-file path str)))

;;; ============================================================
;;; Alignment
;;; ============================================================

(defun align-up (val alignment)
  "Round VAL up to next multiple of ALIGNMENT"
  (let ((rem (mod val alignment)))
    (if (= rem 0)
        val
        (+ val (- alignment rem)))))
