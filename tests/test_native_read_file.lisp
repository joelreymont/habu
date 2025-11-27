;; Tests for native file reading and parsing pipeline
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")

(format t "~%=== Native Read-File Pipeline Tests ===~%~%")

(defvar *pass* 0)
(defvar *fail* 0)

(defun test-code (name source expected)
  (let ((path (format nil "/tmp/rfp_~A" name)))
    (handler-case
        (progn
          (deliver-with-libsystem source path)
          (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" path)
                              :output nil :error nil :wait t)
          (let* ((proc (sb-ext:run-program path nil :output nil :error nil :wait t))
                 (code (sb-ext:process-exit-code proc)))
            (if (= code expected)
                (progn (format t "[PASS] ~A = ~A~%" name code) (incf *pass*))
                (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code) (incf *fail*)))))
      (error (e)
        (format t "[ERR]  ~A: ~A~%" name e)
        (incf *fail*)))))

;; Create test source file: (+ 10 32)
(with-open-file (f "/tmp/test_source.lisp" :direction :output :if-exists :supersede)
  (write-string "(+ 10 32)" f))

;; Test 1: Read file into buffer, get byte count
(test-code "read-file-bytes"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_source.lisp\" 0 0))
            (buf (make-vector 100))
            (n (sys-read fd buf 100)))
       (sys-close fd)
       (if (= n 9) 42 n)))"  ; "(+ 10 32)" is 9 bytes
  42)

;; Test 2: Convert buffer to string and check length
(test-code "buf-to-string-len"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_source.lisp\" 0 0))
            (buf (make-vector 9))
            (n (sys-read fd buf 9)))
       (sys-close fd)
       (let ((str (make-string-from-vector buf)))
         (if (= (string-length str) 9) 42 0))))"
  42)

;; Test 3: Read file and check first character is '('
(test-code "first-char-lparen"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_source.lisp\" 0 0))
            (buf (make-vector 9))
            (n (sys-read fd buf 9)))
       (sys-close fd)
       (let ((str (make-string-from-vector buf)))
         (if (= (string-ref str 0) 40) 42 0))))"  ; '(' = 40
  42)

;; Test 4: Parse file content with reader and check car is symbol
(test-code "parse-car-symbolp"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_source.lisp\" 0 0))
            (buf (make-vector 9))
            (n (sys-read fd buf 9)))
       (sys-close fd)
       (let* ((str (make-string-from-vector buf))
              (expr (read-from-string str)))
         (if (symbolp (car expr)) 42 0))))"
  42)

;; Test 5: Parse and extract second element (10)
(test-code "parse-cadr-value"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_source.lisp\" 0 0))
            (buf (make-vector 9))
            (n (sys-read fd buf 9)))
       (sys-close fd)
       (let* ((str (make-string-from-vector buf))
              (expr (read-from-string str)))
         (cadr expr))))"  ; should be 10
  10)

;; Test 6: Parse and extract third element (32)
(test-code "parse-caddr-value"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_source.lisp\" 0 0))
            (buf (make-vector 9))
            (n (sys-read fd buf 9)))
       (sys-close fd)
       (let* ((str (make-string-from-vector buf))
              (expr (read-from-string str)))
         (caddr expr))))"  ; should be 32
  32)

;; Test 7: Full pipeline - read, parse, compute
(test-code "full-read-parse-compute"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_source.lisp\" 0 0))
            (buf (make-vector 9))
            (n (sys-read fd buf 9)))
       (sys-close fd)
       (let* ((str (make-string-from-vector buf))
              (expr (read-from-string str)))
         (+ (cadr expr) (caddr expr)))))"  ; 10 + 32 = 42
  42)

;; Create test source file with defun
(with-open-file (f "/tmp/test_defun.lisp" :direction :output :if-exists :supersede)
  (write-string "(defun sq (x) (* x x))" f))

;; Test 8: Parse defun form, verify structure
(test-code "parse-defun-name"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_defun.lisp\" 0 0))
            (buf (make-vector 22))
            (n (sys-read fd buf 22)))
       (sys-close fd)
       (let* ((str (make-string-from-vector buf))
              (expr (read-from-string str)))
         (if (symbolp (cadr expr)) 42 0))))"  ; (defun SQ ...) - SQ is symbol
  42)

;; Test 9: Parse defun params, count is 1
(test-code "parse-defun-params"
  "(progn (sys-write 1 \"\" 0)
     (let* ((fd (sys-open \"/tmp/test_defun.lisp\" 0 0))
            (buf (make-vector 22))
            (n (sys-read fd buf 22)))
       (sys-close fd)
       (let* ((str (make-string-from-vector buf))
              (expr (read-from-string str))
              (params (caddr expr)))
         (if (= (length params) 1) 42 0))))"  ; (x) has length 1
  42)

(format t "~%Total: ~A passed, ~A failed~%" *pass* *fail*)
(when (> *fail* 0)
  (sb-ext:exit :code 1))
