;;;; test-fasl.lisp - FASL format tests
;;;;
;;;; Unit tests and property tests for FASL serialization.

(in-package :habu)

;;; ============================================================
;;; Unit Tests
;;; ============================================================

(defun test-fasl-header-roundtrip ()
  "Test that FASL header can be written and read back."
  (let* ((header (make-fasl-header
                  :num-functions 5
                  :code-size 1024
                  :const-pool-size 256
                  :num-relocations 10))
         (path "/tmp/test-fasl-header.bin"))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-fasl-header header out))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (let ((read-header (read-fasl-header in)))
        (if (and (= (fasl-header-magic read-header) +fasl-magic+)
                 (= (fasl-header-version read-header) +fasl-version+)
                 (= (fasl-header-num-functions read-header) 5)
                 (= (fasl-header-code-size read-header) 1024)
                 (= (fasl-header-const-pool-size read-header) 256)
                 (= (fasl-header-num-relocations read-header) 10))
            (progn (format t "  [PASS] fasl-header roundtrip~%") t)
            (progn (format t "  [FAIL] fasl-header roundtrip~%") nil))))))

(defun test-fasl-function-roundtrip ()
  "Test that FASL function entry can be written and read back."
  (let ((path "/tmp/test-fasl-fn.bin"))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (let ((fn (make-fasl-function
                 :name 'test-fn
                 :name-offset 42
                 :code-offset 100
                 :code-size 200
                 :arity 3
                 :flags 1)))
        (write-fasl-function fn out)))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (let ((fn (read-fasl-function in)))
        (if (and (= (fasl-function-name-offset fn) 42)
                 (= (fasl-function-code-offset fn) 100)
                 (= (fasl-function-code-size fn) 200)
                 (= (fasl-function-arity fn) 3)
                 (= (fasl-function-flags fn) 1))
            (progn (format t "  [PASS] fasl-function roundtrip~%") t)
            (progn (format t "  [FAIL] fasl-function roundtrip~%") nil))))))

(defun test-fasl-relocation-roundtrip ()
  "Test that FASL relocation can be written and read back."
  (let ((path "/tmp/test-fasl-reloc.bin"))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (let ((reloc (make-fasl-relocation
                    :type +reloc-fn-call+
                    :offset 128
                    :target 7)))
        (write-fasl-relocation reloc out)))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (let ((reloc (read-fasl-relocation in)))
        (if (and (= (fasl-relocation-type reloc) +reloc-fn-call+)
                 (= (fasl-relocation-offset reloc) 128)
                 (= (fasl-relocation-target reloc) 7))
            (progn (format t "  [PASS] fasl-relocation roundtrip~%") t)
            (progn (format t "  [FAIL] fasl-relocation roundtrip~%") nil))))))

(defun test-string-table ()
  "Test string table building and reading."
  (let ((strings '("foo" "bar" "hello-world")))
    (multiple-value-bind (bytes offsets) (build-string-table strings)
      (let ((bytes-vec (coerce bytes 'vector)))
        (if (and (= (cdr (assoc "foo" offsets :test #'string=)) 0)
                 (string= (read-string-from-table bytes-vec 0) "foo")
                 (string= (read-string-from-table bytes-vec
                            (cdr (assoc "bar" offsets :test #'string=))) "bar")
                 (string= (read-string-from-table bytes-vec
                            (cdr (assoc "hello-world" offsets :test #'string=))) "hello-world"))
            (progn (format t "  [PASS] string-table~%") t)
            (progn (format t "  [FAIL] string-table~%") nil))))))

(defun test-fasl-magic-validation ()
  "Test that invalid magic number is rejected."
  (let ((path "/tmp/test-fasl-bad.bin"))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      ;; Write invalid magic
      (write-u32-le #xDEADBEEF out)
      (dotimes (i 7) (write-u32-le 0 out)))
    (handler-case
        (with-open-file (in path :direction :input
                                 :element-type '(unsigned-byte 8))
          (read-fasl-header in)
          (format t "  [FAIL] fasl-magic-validation (no error)~%")
          nil)
      (error ()
        (format t "  [PASS] fasl-magic-validation~%")
        t))))

;;; ============================================================
;;; Property Tests
;;; ============================================================

(defun gen-fasl-header ()
  "Generator for FASL headers."
  (make-gen
   (lambda ()
     (make-fasl-header
      :num-functions (random 100)
      :code-size (random 65536)
      :const-pool-size (random 4096)
      :num-relocations (random 200)))
   (lambda (h)
     (list (make-fasl-header
            :num-functions 0
            :code-size 0
            :const-pool-size 0
            :num-relocations 0)))))

(defun gen-fasl-function-entry ()
  "Generator for FASL function entries."
  (make-gen
   (lambda ()
     (make-fasl-function
      :name (nth (random 5) '(foo bar baz quux test))
      :name-offset (random 1000)
      :code-offset (random 65536)
      :code-size (random 4096)
      :arity (random 8)
      :flags (random 16)))
   (lambda (f)
     (list (make-fasl-function
            :name 'f
            :name-offset 0
            :code-offset 0
            :code-size 0
            :arity 0
            :flags 0)))))

(defun gen-fasl-relocation-entry ()
  "Generator for FASL relocation entries."
  (make-gen
   (lambda ()
     (make-fasl-relocation
      :type (1+ (random 3))
      :offset (random 65536)
      :target (random 100)))
   (lambda (r)
     (list (make-fasl-relocation :type 1 :offset 0 :target 0)))))

(defproperty prop-fasl-header-roundtrip ((h (gen-fasl-header)))
  (let ((path "/tmp/prop-fasl-header.bin"))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-fasl-header h out))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (let ((h2 (read-fasl-header in)))
        (and (= (fasl-header-magic h2) +fasl-magic+)
             (= (fasl-header-version h2) +fasl-version+)
             (= (fasl-header-num-functions h2) (fasl-header-num-functions h))
             (= (fasl-header-code-size h2) (fasl-header-code-size h))
             (= (fasl-header-const-pool-size h2) (fasl-header-const-pool-size h))
             (= (fasl-header-num-relocations h2) (fasl-header-num-relocations h)))))))

(defproperty prop-fasl-function-roundtrip ((f (gen-fasl-function-entry)))
  (let ((path "/tmp/prop-fasl-fn.bin"))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-fasl-function f out))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (let ((f2 (read-fasl-function in)))
        (and (= (fasl-function-name-offset f2) (fasl-function-name-offset f))
             (= (fasl-function-code-offset f2) (fasl-function-code-offset f))
             (= (fasl-function-code-size f2) (fasl-function-code-size f))
             (= (fasl-function-arity f2) (fasl-function-arity f))
             (= (fasl-function-flags f2) (fasl-function-flags f)))))))

(defproperty prop-fasl-relocation-roundtrip ((r (gen-fasl-relocation-entry)))
  (let ((path "/tmp/prop-fasl-reloc.bin"))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-fasl-relocation r out))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (let ((r2 (read-fasl-relocation in)))
        (and (= (fasl-relocation-type r2) (fasl-relocation-type r))
             (= (fasl-relocation-offset r2) (fasl-relocation-offset r))
             (= (fasl-relocation-target r2) (fasl-relocation-target r)))))))

(defproperty prop-string-table-roundtrip ((s (gen-one-of '("a" "test" "hello" "world" "foo-bar"))))
  (multiple-value-bind (bytes offsets) (build-string-table (list s))
    (let ((bytes-vec (coerce bytes 'vector))
          (offset (cdr (first offsets))))
      (string= s (read-string-from-table bytes-vec offset)))))

(defproperty prop-fasl-header-size ((h (gen-fasl-header)))
  "FASL header is always 32 bytes (8 x 4-byte fields)."
  (let ((path "/tmp/prop-fasl-size.bin"))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-fasl-header h out))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (= (file-length in) 32))))

(defproperty prop-fasl-function-size ((f (gen-fasl-function-entry)))
  "FASL function entry is always 20 bytes (5 x 4-byte fields)."
  (let ((path "/tmp/prop-fasl-fn-size.bin"))
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-fasl-function f out))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (= (file-length in) 20))))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-fasl-tests (&optional (trials *quickcheck-trials*))
  "Run all FASL tests."
  (format t "~%=== FASL Tests (~D trials each) ===~%~%" trials)
  (reset-property-stats)

  (let ((unit-pass 0) (unit-fail 0))
    ;; Unit tests
    (format t "FASL unit tests:~%")
    (if (test-fasl-header-roundtrip) (incf unit-pass) (incf unit-fail))
    (if (test-fasl-function-roundtrip) (incf unit-pass) (incf unit-fail))
    (if (test-fasl-relocation-roundtrip) (incf unit-pass) (incf unit-fail))
    (if (test-string-table) (incf unit-pass) (incf unit-fail))
    (if (test-fasl-magic-validation) (incf unit-pass) (incf unit-fail))

    ;; Property tests
    (format t "~%FASL property tests:~%")
    (run-property 'prop-fasl-header-roundtrip trials)
    (run-property 'prop-fasl-function-roundtrip trials)
    (run-property 'prop-fasl-relocation-roundtrip trials)
    (run-property 'prop-string-table-roundtrip trials)
    (run-property 'prop-fasl-header-size trials)
    (run-property 'prop-fasl-function-size trials)

    ;; Summary
    (format t "~%FASL Tests: ~D unit + ~D property = ~D passed, ~D failed~%"
            unit-pass *property-pass-count*
            (+ unit-pass *property-pass-count*)
            (+ unit-fail *property-fail-count*))

    (values (and (= unit-fail 0) (= *property-fail-count* 0))
            (+ unit-pass *property-pass-count*)
            (+ unit-fail *property-fail-count*))))

;;; Run tests when file is loaded
(run-fasl-tests)
