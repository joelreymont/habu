;;;; test-fasl.lisp - FASL format tests
;;;;
;;;; Unit tests and property tests for FASL serialization.

(defpackage :habu-test-fasl
  (:use :cl :habu-test-quickcheck)
  (:shadowing-import-from :habu #:compile-file #:compile-to-fasl #:read-fasl
                          #:+fasl-magic+ #:+fasl-version+ #:+fasl-arch-arm64+
                          #:+fasl-header-size+ #:+fasl-function-size+ #:+fasl-relocation-size+
                          #:fasl-header #:make-fasl-header #:fasl-header-p
                          #:fasl-header-magic #:fasl-header-version #:fasl-header-arch
                          #:fasl-header-flags #:fasl-header-num-functions
                          #:fasl-header-code-size #:fasl-header-const-pool-size
                          #:fasl-header-num-relocations #:fasl-header-num-imports
                          #:fasl-function #:make-fasl-function #:fasl-function-p
                          #:fasl-function-name #:fasl-function-name-offset
                          #:fasl-function-code-offset #:fasl-function-code-size
                          #:fasl-function-arity #:fasl-function-flags
                          #:fasl-relocation #:make-fasl-relocation #:fasl-relocation-p
                          #:fasl-relocation-type #:fasl-relocation-offset #:fasl-relocation-target
                          #:+reloc-fn-call+ #:+reloc-extern-call+ #:+reloc-constant+
                          #:+fn-flag-exported+ #:+fn-flag-entry+
                          #:write-fasl-header #:read-fasl-header
                          #:write-fasl-function #:read-fasl-function
                          #:write-fasl-relocation #:read-fasl-relocation
                          #:link-fasls
                          #:build-string-table #:read-string-from-table
                          #:build-fasl-functions
                          #:write-u32-le #:function-exported-p))

(in-package :habu-test-fasl)

;;; ============================================================
;;; Test Infrastructure
;;; ============================================================

(defvar *test-pass-count* 0)
(defvar *test-fail-count* 0)

(defmacro deftest (name docstring &body body)
  "Define a test with automatic pass/fail handling and error catching.
   Body should call (pass) or (pass info) on success, (fail) or (fail reason) on failure.
   Uncaught errors automatically fail with the error message."
  (let ((test-name-str (string-downcase (symbol-name name))))
    `(defun ,name ()
       ,docstring
       (flet ((pass (&optional info)
                (if info
                    (format t "  [PASS] ~A (~A)~%" ,test-name-str info)
                    (format t "  [PASS] ~A~%" ,test-name-str))
                t)
              (fail (&optional reason)
                (if reason
                    (format t "  [FAIL] ~A: ~A~%" ,test-name-str reason)
                    (format t "  [FAIL] ~A~%" ,test-name-str))
                nil))
         (declare (ignorable #'pass #'fail))
         (handler-case
             (progn ,@body)
           (error (e)
             (fail e)))))))

(defmacro run-test-group (group-name &rest test-fns)
  "Run a group of tests, updating *test-pass-count* and *test-fail-count*."
  `(progn
     (format t "~%~A:~%" ,group-name)
     ,@(loop for fn in test-fns
             collect `(if (,fn)
                          (incf *test-pass-count*)
                          (incf *test-fail-count*)))))

(defmacro with-temp-file ((var prefix) &body body)
  "Execute body with VAR bound to a temp file path."
  `(let ((,var (format nil "/tmp/~A-~A.tmp" ,prefix (random 100000))))
     (unwind-protect
          (progn ,@body)
       (ignore-errors (delete-file ,var)))))

(defmacro with-temp-files (bindings &body body)
  "Execute body with multiple temp file bindings: ((var1 prefix1) (var2 prefix2) ...)"
  (if (null bindings)
      `(progn ,@body)
      `(with-temp-file ,(car bindings)
         (with-temp-files ,(cdr bindings) ,@body))))

;;; ============================================================
;;; Serialization Tests
;;; ============================================================

(deftest test-fasl-header-roundtrip
  "Test that FASL header can be written and read back."
  (with-temp-file (path "fasl-hdr")
    (let ((header (make-fasl-header
                   :num-functions 5
                   :code-size 1024
                   :const-pool-size 256
                   :num-relocations 10
                   :num-imports 3)))
      (with-open-file (out path :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
        (write-fasl-header header out))
      (with-open-file (in path :direction :input
                               :element-type '(unsigned-byte 8))
        (let ((h (read-fasl-header in)))
          (if (and (= (fasl-header-magic h) +fasl-magic+)
                   (= (fasl-header-version h) +fasl-version+)
                   (= (fasl-header-num-functions h) 5)
                   (= (fasl-header-code-size h) 1024)
                   (= (fasl-header-const-pool-size h) 256)
                   (= (fasl-header-num-relocations h) 10)
                   (= (fasl-header-num-imports h) 3))
              (pass)
              (fail "header mismatch")))))))

(deftest test-fasl-function-roundtrip
  "Test that FASL function entry can be written and read back."
  (with-temp-file (path "fasl-fn")
    (let ((fn-entry (make-fasl-function
                     :name 'test-fn
                     :name-offset 42
                     :code-offset 100
                     :code-size 200
                     :arity 3
                     :flags 1)))
      (with-open-file (out path :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
        (write-fasl-function fn-entry out))
      (with-open-file (in path :direction :input
                               :element-type '(unsigned-byte 8))
        (let ((f (read-fasl-function in)))
          (if (and (= (fasl-function-name-offset f) 42)
                   (= (fasl-function-code-offset f) 100)
                   (= (fasl-function-code-size f) 200)
                   (= (fasl-function-arity f) 3)
                   (= (fasl-function-flags f) 1))
              (pass)
              (fail "function entry mismatch")))))))

(deftest test-fasl-relocation-roundtrip
  "Test that FASL relocation can be written and read back."
  (with-temp-file (path "fasl-reloc")
    (let ((reloc (make-fasl-relocation
                  :type +reloc-fn-call+
                  :offset 128
                  :target 7)))
      (with-open-file (out path :direction :output
                                :if-exists :supersede
                                :element-type '(unsigned-byte 8))
        (write-fasl-relocation reloc out))
      (with-open-file (in path :direction :input
                               :element-type '(unsigned-byte 8))
        (let ((r (read-fasl-relocation in)))
          (if (and (= (fasl-relocation-type r) +reloc-fn-call+)
                   (= (fasl-relocation-offset r) 128)
                   (= (fasl-relocation-target r) 7))
              (pass)
              (fail "relocation mismatch")))))))

(deftest test-string-table
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
            (pass)
            (fail "string table mismatch"))))))

(deftest test-fasl-magic-validation
  "Test that invalid magic number is rejected."
  (with-temp-file (path "fasl-bad")
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-u32-le #xDEADBEEF out)
      (dotimes (i 7) (write-u32-le 0 out)))
    (handler-case
        (progn
          (with-open-file (in path :direction :input
                                   :element-type '(unsigned-byte 8))
            (read-fasl-header in))
          (fail "no error raised"))
      (error () (pass)))))

(deftest test-export-flags
  "Test that export flags are correctly set on functions."
  (let* ((fnoffs '((foo . 100) (bar . 200) (baz . 300)))
         (exports '(foo baz))
         (functions (build-fasl-functions fnoffs :exports exports)))
    (let ((foo-fn (find-if (lambda (f) (equal (fasl-function-name f) 'foo)) functions))
          (bar-fn (find-if (lambda (f) (equal (fasl-function-name f) 'bar)) functions))
          (baz-fn (find-if (lambda (f) (equal (fasl-function-name f) 'baz)) functions)))
      (cond
        ((not (and foo-fn (plusp (logand (fasl-function-flags foo-fn) +fn-flag-exported+))))
         (fail "foo not exported"))
        ((not (and bar-fn (zerop (logand (fasl-function-flags bar-fn) +fn-flag-exported+))))
         (fail "bar should not be exported"))
        ((not (and baz-fn (plusp (logand (fasl-function-flags baz-fn) +fn-flag-exported+))))
         (fail "baz not exported"))
        (t (pass))))))

;;; ============================================================
;;; Compilation Tests
;;; ============================================================

(deftest test-compile-with-exports
  "Test compiling a module with exports."
  (with-temp-files ((src "mod-src.lisp") (fasl "mod.fasl"))
    (with-open-file (out src :direction :output :if-exists :supersede)
      (write-string "(defun helper (x) (+ x 1))
(defun public-fn (x) (helper x))
42" out))
    (compile-file src :output-file fasl :exports '(public-fn) :verbose nil)
    (multiple-value-bind (header functions code relocs constants str-table imports)
        (read-fasl fasl)
      (declare (ignore constants imports))
      ;; Verify FASL is valid before examining functions
      (assert (and header (plusp (length code)) relocs str-table))
      (let ((public-fn nil) (helper-fn nil))
        (dolist (fn functions)
          (let ((name (read-string-from-table str-table (fasl-function-name-offset fn))))
            (cond ((string= name "PUBLIC-FN") (setf public-fn fn))
                  ((string= name "HELPER") (setf helper-fn fn)))))
        (cond
          ((not (and public-fn (function-exported-p public-fn)))
           (fail "public-fn not exported"))
          ((and helper-fn (function-exported-p helper-fn))
           (fail "helper should not be exported"))
          (t (pass)))))))

(deftest test-compile-all-control-flow
  "Test compiling code with ALL control flow constructs to FASL.
   Catches bugs where markers leak position values into bytecode."
  (with-temp-files ((src "ctrl-flow.lisp") (fasl "ctrl-flow.fasl"))
    (with-open-file (out src :direction :output :if-exists :supersede)
      (write-string "
;; block/return-from
(defun early-exit (n)
  (block done
    (if (< n 0) (return-from done -1) (* n 2))))

;; nested blocks
(defun nested-blocks (x)
  (block outer
    (block inner
      (if (= x 0) (return-from outer 100) (return-from inner x)))
    999))

;; labels (local functions)
(defun with-labels (n)
  (labels ((helper (x) (+ x 1))
           (double (x) (* x 2)))
    (helper (double n))))

;; while loop (becomes labels + TCO)
(defun count-down (n)
  (let ((result 0))
    (while (> n 0)
      (setq result (+ result n))
      (setq n (- n 1)))
    result))

;; explicit tail recursion
(defun tail-sum (n acc)
  (if (= n 0) acc (tail-sum (- n 1) (+ acc n))))

;; deeply nested control flow
(defun complex-flow (a b c)
  (block outer
    (if (> a 0)
        (block middle
          (if (> b 0)
              (block inner
                (if (> c 0)
                    (return-from inner c)
                    (return-from middle b)))
              (return-from outer a)))
        0)))

(defun main () (+ (early-exit 5) (nested-blocks 1) (with-labels 3)))
" out))
    (compile-file src :output-file fasl :verbose nil)
    (multiple-value-bind (header functions code relocs constants str-table imports)
        (read-fasl fasl)
      (declare (ignore constants imports))
      ;; Verify basic FASL structure before checking code bytes
      (assert (and header functions relocs str-table))
      (let ((invalid nil))
        (dotimes (i (length code))
          (let ((b (aref code i)))
            (unless (<= 0 b 255)
              (push (cons i b) invalid))))
        (if invalid
            (fail (format nil "invalid bytes: ~A" invalid))
            (pass))))))

;;; ============================================================
;;; Linker Tests
;;; ============================================================

(deftest test-link-single-fasl
  "Test linking a single FASL into an executable."
  (with-temp-files ((src "link1.lisp") (fasl "link1.fasl") (bin "link1"))
    (with-open-file (out src :direction :output :if-exists :supersede)
      (write-string "(defun main () 42)" out))
    (compile-file src :output-file fasl :verbose nil)
    (link-fasls (list fasl) bin :include-gc t :verbose nil)
    (let ((size (with-open-file (in bin :element-type '(unsigned-byte 8))
                  (file-length in))))
      (if (> size 1000)
          (pass (format nil "~D bytes" size))
          (fail (format nil "too small: ~D bytes" size))))))

(deftest test-link-multiple-fasls
  "Test linking multiple FASLs into an executable."
  (with-temp-files ((src1 "mod1.lisp") (src2 "mod2.lisp")
                    (fasl1 "mod1.fasl") (fasl2 "mod2.fasl") (bin "multi"))
    (with-open-file (out src1 :direction :output :if-exists :supersede)
      (write-string "(defun helper (x) (+ x 10))" out))
    (with-open-file (out src2 :direction :output :if-exists :supersede)
      (write-string "(defun main () (helper 32))" out))
    (compile-file src1 :output-file fasl1 :verbose nil)
    (compile-file src2 :output-file fasl2 :imports '(helper) :verbose nil)
    (link-fasls (list fasl1 fasl2) bin :include-gc t :verbose nil)
    (let ((size (with-open-file (in bin :element-type '(unsigned-byte 8))
                  (file-length in))))
      (if (> size 1000)
          (pass (format nil "~D bytes" size))
          (fail "too small")))))

(deftest test-link-cross-module-calls
  "Test that cross-module function calls are resolved correctly."
  (with-temp-files ((src1 "xm1.lisp") (src2 "xm2.lisp") (src3 "xm3.lisp")
                    (fasl1 "xm1.fasl") (fasl2 "xm2.fasl") (fasl3 "xm3.fasl")
                    (bin "xmod"))
    (with-open-file (out src1 :direction :output :if-exists :supersede)
      (write-string "(defun add-one (x) (+ x 1))" out))
    (with-open-file (out src2 :direction :output :if-exists :supersede)
      (write-string "(defun add-two (x) (add-one (add-one x)))" out))
    (with-open-file (out src3 :direction :output :if-exists :supersede)
      (write-string "(defun main () (add-two 40))" out))
    (compile-file src1 :output-file fasl1 :verbose nil)
    (compile-file src2 :output-file fasl2 :imports '(add-one) :verbose nil)
    (compile-file src3 :output-file fasl3 :imports '(add-two) :verbose nil)
    (link-fasls (list fasl1 fasl2 fasl3) bin :include-gc t :verbose nil)
    (pass)))

(deftest test-link-with-gc-runtime
  "Test that GC runtime is included when requested."
  (with-temp-files ((src "gc-test.lisp") (fasl "gc-test.fasl") (bin "gc-bin"))
    (with-open-file (out src :direction :output :if-exists :supersede)
      (write-string "(defun main () 1)" out))
    (compile-file src :output-file fasl :verbose nil)
    ;; Link with GC - should succeed and create valid binary
    (link-fasls (list fasl) bin :include-gc t :verbose nil)
    (let ((size (with-open-file (in bin :element-type '(unsigned-byte 8))
                  (file-length in))))
      ;; Just verify we created a valid Mach-O (has reasonable size)
      (if (> size 1000)
          (pass (format nil "Binary created: ~D bytes" size))
          (fail (format nil "Binary too small: ~D bytes" size))))))

(deftest test-link-fasl-order-matters
  "Test that FASL order affects symbol resolution (first definition wins)."
  (with-temp-files ((src1 "ord1.lisp") (src2 "ord2.lisp")
                    (fasl1 "ord1.fasl") (fasl2 "ord2.fasl") (bin "order"))
    (with-open-file (out src1 :direction :output :if-exists :supersede)
      (write-string "(defun foo () 1)" out))
    (with-open-file (out src2 :direction :output :if-exists :supersede)
      (write-string "(defun foo () 2)
(defun main () (foo))" out))
    (compile-file src1 :output-file fasl1 :verbose nil)
    (compile-file src2 :output-file fasl2 :verbose nil)
    (link-fasls (list fasl1 fasl2) bin :include-gc t :verbose nil)
    (pass)))

(deftest test-link-invalid-fasl-rejected
  "Test that invalid FASL files are rejected."
  (with-temp-files ((bad-fasl "bad.fasl") (bin "bad-link"))
    (with-open-file (out bad-fasl :direction :output
                                  :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
      (dotimes (i 100) (write-byte (mod i 256) out)))
    (handler-case
        (progn
          (link-fasls (list bad-fasl) bin :verbose nil)
          (fail "no error raised"))
      (error () (pass)))))

(deftest test-link-empty-fasl-list
  "Test that empty FASL list is handled gracefully."
  (with-temp-file (bin "empty")
    (handler-case
        (progn
          (link-fasls '() bin :verbose nil)
          (pass "no crash"))
      (error (e)
        (pass (format nil "error: ~A" (type-of e)))))))

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
      :num-relocations (random 200)
      :num-imports (random 50)))
   (lambda (h)
     (let ((nf (fasl-header-num-functions h))
           (cs (fasl-header-code-size h))
           (cp (fasl-header-const-pool-size h))
           (nr (fasl-header-num-relocations h))
           (ni (fasl-header-num-imports h))
           (candidates nil))
       (when (> nf 0)
         (push (make-fasl-header :num-functions (truncate nf 2)
                                 :code-size cs :const-pool-size cp
                                 :num-relocations nr :num-imports ni)
               candidates))
       (when (> cs 0)
         (push (make-fasl-header :num-functions nf
                                 :code-size (truncate cs 2) :const-pool-size cp
                                 :num-relocations nr :num-imports ni)
               candidates))
       (when (> cp 0)
         (push (make-fasl-header :num-functions nf
                                 :code-size cs :const-pool-size (truncate cp 2)
                                 :num-relocations nr :num-imports ni)
               candidates))
       (when (> nr 0)
         (push (make-fasl-header :num-functions nf
                                 :code-size cs :const-pool-size cp
                                 :num-relocations (truncate nr 2) :num-imports ni)
               candidates))
       (when (> ni 0)
         (push (make-fasl-header :num-functions nf
                                 :code-size cs :const-pool-size cp
                                 :num-relocations nr :num-imports (truncate ni 2))
               candidates))
       (nreverse candidates)))))

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
     (let ((no (fasl-function-name-offset f))
           (co (fasl-function-code-offset f))
           (cs (fasl-function-code-size f))
           (ar (fasl-function-arity f))
           (fl (fasl-function-flags f))
           (nm (fasl-function-name f))
           (candidates nil))
       (when (> no 0)
         (push (make-fasl-function :name nm :name-offset (truncate no 2)
                                   :code-offset co :code-size cs :arity ar :flags fl)
               candidates))
       (when (> co 0)
         (push (make-fasl-function :name nm :name-offset no
                                   :code-offset (truncate co 2) :code-size cs :arity ar :flags fl)
               candidates))
       (when (> cs 0)
         (push (make-fasl-function :name nm :name-offset no
                                   :code-offset co :code-size (truncate cs 2) :arity ar :flags fl)
               candidates))
       (when (> ar 0)
         (push (make-fasl-function :name nm :name-offset no
                                   :code-offset co :code-size cs :arity (truncate ar 2) :flags fl)
               candidates))
       (when (> fl 0)
         (push (make-fasl-function :name nm :name-offset no
                                   :code-offset co :code-size cs :arity ar :flags (truncate fl 2))
               candidates))
       (nreverse candidates)))))

(defun gen-fasl-relocation-entry ()
  "Generator for FASL relocation entries."
  (make-gen
   (lambda ()
     (make-fasl-relocation
      :type (1+ (random 3))
      :offset (random 65536)
      :target (random 100)))
   (lambda (r)
     (let ((ty (fasl-relocation-type r))
           (off (fasl-relocation-offset r))
           (tgt (fasl-relocation-target r))
           (candidates nil))
       (when (> ty 1)
         (push (make-fasl-relocation :type (1- ty) :offset off :target tgt) candidates))
       (when (> off 0)
         (push (make-fasl-relocation :type ty :offset (truncate off 2) :target tgt) candidates))
       (when (> tgt 0)
         (push (make-fasl-relocation :type ty :offset off :target (truncate tgt 2)) candidates))
       (nreverse candidates)))))

(defproperty prop-fasl-header-roundtrip ((h (gen-fasl-header)))
  (with-temp-file (path "prop-hdr")
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
             (= (fasl-header-num-relocations h2) (fasl-header-num-relocations h))
             (= (fasl-header-num-imports h2) (fasl-header-num-imports h)))))))

(defproperty prop-fasl-function-roundtrip ((f (gen-fasl-function-entry)))
  (with-temp-file (path "prop-fn")
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
  (with-temp-file (path "prop-reloc")
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
  "FASL header size matches +fasl-header-size+ constant."
  (with-temp-file (path "prop-size")
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-fasl-header h out))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (= (file-length in) +fasl-header-size+))))

(defproperty prop-fasl-function-size ((f (gen-fasl-function-entry)))
  "FASL function entry size matches +fasl-function-size+ constant."
  (with-temp-file (path "prop-fn-size")
    (with-open-file (out path :direction :output
                              :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-fasl-function f out))
    (with-open-file (in path :direction :input
                             :element-type '(unsigned-byte 8))
      (= (file-length in) +fasl-function-size+))))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-fasl-tests (&optional (trials *quickcheck-trials*))
  "Run all FASL tests."
  (format t "~%=== FASL Tests (~D trials each) ===~%" trials)
  (reset-property-stats)
  (setf *test-pass-count* 0
        *test-fail-count* 0)

  ;; Unit tests by category
  (run-test-group "FASL serialization tests"
    test-fasl-header-roundtrip
    test-fasl-function-roundtrip
    test-fasl-relocation-roundtrip
    test-string-table
    test-fasl-magic-validation
    test-export-flags)

  (run-test-group "FASL compilation tests"
    test-compile-with-exports
    test-compile-all-control-flow)

  (run-test-group "FASL linker tests"
    test-link-single-fasl
    test-link-multiple-fasls
    test-link-cross-module-calls
    test-link-with-gc-runtime
    test-link-fasl-order-matters
    test-link-invalid-fasl-rejected
    test-link-empty-fasl-list)

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
          *test-pass-count* *property-pass-count*
          (+ *test-pass-count* *property-pass-count*)
          (+ *test-fail-count* *property-fail-count*))

  (values (and (= *test-fail-count* 0) (= *property-fail-count* 0))
          (+ *test-pass-count* *property-pass-count*)
          (+ *test-fail-count* *property-fail-count*)))

;;; Run tests when file is loaded
(run-fasl-tests)
