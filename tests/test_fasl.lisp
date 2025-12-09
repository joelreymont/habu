;;; Test FASL file format - compile to FASL and execute

(load "bootstrap/compiler.lisp")

(defpackage :habu-test-fasl
  (:use :cl)
  (:import-from :habu #:read-all #:compile-program))
(in-package :habu-test-fasl)

;;; FASL format constants
(defconstant +fasl-magic+ #x4C534648)  ; "HFSL" in little-endian
(defconstant +fasl-version+ 1)

(defun write-u32-le (stream value)
  "Write a 32-bit unsigned integer in little-endian format"
  (write-byte (logand value #xFF) stream)
  (write-byte (logand (ash value -8) #xFF) stream)
  (write-byte (logand (ash value -16) #xFF) stream)
  (write-byte (logand (ash value -24) #xFF) stream))

(defun read-u32-le (stream)
  "Read a 32-bit unsigned integer in little-endian format"
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream)))
    (logior b0 (ash b1 8) (ash b2 16) (ash b3 24))))

(defun write-fasl (code-bytes output-path)
  "Write compiled code to a FASL file"
  (with-open-file (out output-path
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
    (write-u32-le out +fasl-magic+)
    (write-u32-le out +fasl-version+)
    (write-u32-le out 0)  ; flags (reserved)
    (write-u32-le out (length code-bytes))
    (dolist (byte code-bytes)
      (write-byte byte out)))
  output-path)

(defun read-fasl (input-path)
  "Read a FASL file and return the code bytes"
  (with-open-file (in input-path
                      :direction :input
                      :element-type '(unsigned-byte 8))
    (let ((magic (read-u32-le in)))
      (unless (= magic +fasl-magic+)
        (error "Not a valid FASL file: ~A" input-path)))
    (let ((version (read-u32-le in)))
      (unless (<= version +fasl-version+)
        (error "Unsupported FASL version: ~A" version)))
    (read-u32-le in)  ; skip flags
    (let* ((code-len (read-u32-le in))
           (code (make-array code-len :element-type '(unsigned-byte 8))))
      (read-sequence code in)
      (coerce code 'list))))

(defun compile-to-fasl (source-string output-path)
  "Compile Lisp source string to FASL file"
  (let* ((forms (read-all source-string))
         (code (compile-program forms nil)))
    (write-fasl code output-path)))

(defvar *tests-passed* 0)
(defvar *tests-run* 0)

(defmacro test (name expected &body forms)
  `(progn
     (incf *tests-run*)
     (handler-case
         (let ((result (progn ,@forms)))
           (if (equal result ,expected)
               (progn
                 (format t "  ~A... ok~%" ,name)
                 (incf *tests-passed*))
               (format t "  ~A... FAIL (expected ~A, got ~A)~%"
                       ,name ,expected result)))
       (error (e)
         (format t "  ~A... ERROR: ~A~%" ,name e)))))

(format t "FASL format tests:~%")

;;; Test write-fasl and read-fasl round-trip
(test "write-fasl/read-fasl round-trip"
      '(1 2 3 4 5)
      (let ((test-bytes '(1 2 3 4 5))
            (test-path "/tmp/test_round_trip.fasl"))
        (write-fasl test-bytes test-path)
        (read-fasl test-path)))

;;; Test FASL magic header
(test "FASL magic header"
      t
      (let ((test-path "/tmp/test_magic.fasl"))
        (write-fasl '(0 0 0 0) test-path)
        (with-open-file (in test-path :element-type '(unsigned-byte 8))
          ;; Read magic: "HFSL" = 0x48 0x46 0x53 0x4C little-endian
          (let ((b0 (read-byte in))
                (b1 (read-byte in))
                (b2 (read-byte in))
                (b3 (read-byte in)))
            ;; H=0x48, F=0x46, S=0x53, L=0x4C
            (and (= b0 #x48) (= b1 #x46) (= b2 #x53) (= b3 #x4C))))))

;;; Test compile-to-fasl produces FASL
(test "compile-to-fasl produces FASL"
      t
      (let ((fasl-path "/tmp/test_add.fasl"))
        (compile-to-fasl "(+ 10 20)" fasl-path)
        (and (probe-file fasl-path)
             (with-open-file (in fasl-path :element-type '(unsigned-byte 8))
               (= (read-u32-le in) +fasl-magic+)))))

;;; Test FASL version
(test "FASL version is 1"
      1
      (let ((test-path "/tmp/test_version.fasl"))
        (write-fasl '(0 0 0 0) test-path)
        (with-open-file (in test-path :element-type '(unsigned-byte 8))
          (read-u32-le in)  ; skip magic
          (read-u32-le in)))) ; read version

;;; Test code length stored correctly
(test "FASL code length"
      100
      (let ((test-bytes (loop for i from 0 below 100 collect i))
            (test-path "/tmp/test_length.fasl"))
        (write-fasl test-bytes test-path)
        (with-open-file (in test-path :element-type '(unsigned-byte 8))
          (read-u32-le in)  ; skip magic
          (read-u32-le in)  ; skip version
          (read-u32-le in)  ; skip flags
          (read-u32-le in)))) ; read code length

;;; Test FASL execution via run-fasl
(test "FASL execution: simple addition"
      30
      (let ((fasl-path "/tmp/test_exec_add.fasl"))
        (compile-to-fasl "(+ 10 20)" fasl-path)
        ;; Run with run-fasl and extract result
        (let* ((output (with-output-to-string (s)
                         (sb-ext:run-program "./run-fasl"
                                             (list fasl-path)
                                             :output s :error :output)))
               ;; Parse "=> fixnum N"
               (pos (search "=> fixnum " output)))
          (when pos
            (parse-integer output :start (+ pos 10) :junk-allowed t)))))

;;; Test FASL execution with function
(test "FASL execution: factorial"
      720
      (let ((fasl-path "/tmp/test_exec_fact.fasl"))
        (compile-to-fasl "(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 6)"
                         fasl-path)
        (let* ((output (with-output-to-string (s)
                         (sb-ext:run-program "./run-fasl"
                                             (list fasl-path)
                                             :output s :error :output)))
               (pos (search "=> fixnum " output)))
          (when pos
            (parse-integer output :start (+ pos 10) :junk-allowed t)))))

(format t "~%~A/~A tests passed~%" *tests-passed* *tests-run*)
(sb-ext:exit :code (if (= *tests-passed* *tests-run*) 0 1))
