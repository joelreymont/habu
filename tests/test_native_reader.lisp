;;; Tests for native reader primitives
(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "macho-linker.lisp")

(format t "~%=== Native Reader Primitives Tests ===~%~%")

(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defun test-libsystem-code (name source expected-code)
  "Test deliver-with-libsystem: builds executable, runs it, checks exit code only."
  (handler-case
    (let ((output-path (format nil "/tmp/reader_~A" name)))
      (deliver-with-libsystem source output-path)
      (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" output-path)
                          :output nil :error nil :wait t)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (code (sb-ext:process-exit-code proc)))
        (if (= code expected-code)
            (progn (format t "[PASS] ~A = ~A~%" name code)
                   (incf *pass-count*))
            (progn (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected-code code)
                   (incf *fail-count*)))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e)
      (incf *fail-count*))))

;;; Character predicates

;; whitespace? - space, tab, newline, carriage return
(test-libsystem-code "whitespace-space"
  "(defun whitespace? (ch) (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))
   (if (whitespace? #x20) 42 0)"
  42)

(test-libsystem-code "whitespace-tab"
  "(defun whitespace? (ch) (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))
   (if (whitespace? #x09) 42 0)"
  42)

(test-libsystem-code "whitespace-non"
  "(defun whitespace? (ch) (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))
   (if (whitespace? #x41) 42 0)"
  0)

;; digit?
(test-libsystem-code "digit-0"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (if (digit? #x30) 42 0)"
  42)

(test-libsystem-code "digit-9"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (if (digit? #x39) 42 0)"
  42)

(test-libsystem-code "digit-non"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (if (digit? #x41) 42 0)"
  0)

;; hex-digit?
(test-libsystem-code "hex-digit-5"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun hex-digit? (ch) (or (digit? ch) (and (>= ch #x41) (<= ch #x46)) (and (>= ch #x61) (<= ch #x66))))
   (if (hex-digit? #x35) 42 0)"
  42)

(test-libsystem-code "hex-digit-A"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun hex-digit? (ch) (or (digit? ch) (and (>= ch #x41) (<= ch #x46)) (and (>= ch #x61) (<= ch #x66))))
   (if (hex-digit? #x41) 42 0)"
  42)

(test-libsystem-code "hex-digit-a"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun hex-digit? (ch) (or (digit? ch) (and (>= ch #x41) (<= ch #x46)) (and (>= ch #x61) (<= ch #x66))))
   (if (hex-digit? #x61) 42 0)"
  42)

;;; Low-level string/char access

;; char-at - access character in string
(test-libsystem-code "char-at-first"
  "(defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (char-at \"ABC\" 0)"
  65)

(test-libsystem-code "char-at-mid"
  "(defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (char-at \"ABC\" 1)"
  66)

(test-libsystem-code "char-at-end"
  "(defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (char-at \"ABC\" 3)"
  0)

;; digit-val and hex-val
(test-libsystem-code "digit-val"
  "(defun digit-val (ch) (- ch #x30))
   (digit-val #x35)"
  5)

(test-libsystem-code "hex-val-digit"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun hex-val (ch) (cond ((digit? ch) (- ch #x30)) ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) #xA)) ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) #xA)) (t #x0)))
   (hex-val #x35)"
  5)

(test-libsystem-code "hex-val-upper"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun hex-val (ch) (cond ((digit? ch) (- ch #x30)) ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) #xA)) ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) #xA)) (t #x0)))
   (hex-val #x43)"
  12)

(test-libsystem-code "hex-val-lower"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun hex-val (ch) (cond ((digit? ch) (- ch #x30)) ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) #xA)) ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) #xA)) (t #x0)))
   (hex-val #x63)"
  12)

;;; Number parsing

;; read-digits - parse decimal digits
(test-libsystem-code "read-digits-single"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun digit-val (ch) (- ch #x30))
   (defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (defun read-digits (source pos n)
     (let ((ch (char-at source pos)))
       (if (digit? ch)
           (read-digits source (+ pos 1) (+ (* n 10) (digit-val ch)))
           (cons n pos))))
   (car (read-digits \"5\" 0 0))"
  5)

(test-libsystem-code "read-digits-multi"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun digit-val (ch) (- ch #x30))
   (defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (defun read-digits (source pos n)
     (let ((ch (char-at source pos)))
       (if (digit? ch)
           (read-digits source (+ pos 1) (+ (* n 10) (digit-val ch)))
           (cons n pos))))
   (car (read-digits \"123\" 0 0))"
  123)

(test-libsystem-code "read-digits-pos"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun digit-val (ch) (- ch #x30))
   (defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (defun read-digits (source pos n)
     (let ((ch (char-at source pos)))
       (if (digit? ch)
           (read-digits source (+ pos 1) (+ (* n 10) (digit-val ch)))
           (cons n pos))))
   (cdr (read-digits \"42x\" 0 0))"
  2)

;;; String upcase (needed for symbol normalization)

(test-libsystem-code "char-upcase-lower"
  "(defun char-upcase (ch) (if (and (>= ch #x61) (<= ch #x7A)) (- ch #x20) ch))
   (char-upcase #x61)"
  65)

(test-libsystem-code "char-upcase-upper"
  "(defun char-upcase (ch) (if (and (>= ch #x61) (<= ch #x7A)) (- ch #x20) ch))
   (char-upcase #x41)"
  65)

(test-libsystem-code "char-upcase-digit"
  "(defun char-upcase (ch) (if (and (>= ch #x61) (<= ch #x7A)) (- ch #x20) ch))
   (char-upcase #x35)"
  53)

;;; Full integer parsing with sign

(test-libsystem-code "read-int-positive"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun digit-val (ch) (- ch #x30))
   (defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (defun read-digits (source pos n)
     (let ((ch (char-at source pos)))
       (if (digit? ch)
           (read-digits source (+ pos 1) (+ (* n 10) (digit-val ch)))
           (cons n pos))))
   (defun read-int (source pos)
     (let* ((neg nil) (start pos) (ch (char-at source pos)))
       (cond ((= ch #x2D) (setf neg t) (setf start (+ pos 1)))
             ((= ch #x2B) (setf start (+ pos 1))))
       (let ((result (read-digits source start 0)))
         (let ((val (car result)) (end (cdr result)))
           (cons (if neg (- 0 val) val) end)))))
   (car (read-int \"42\" 0))"
  42)

(test-libsystem-code "read-int-negative"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun digit-val (ch) (- ch #x30))
   (defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (defun read-digits (source pos n)
     (let ((ch (char-at source pos)))
       (if (digit? ch)
           (read-digits source (+ pos 1) (+ (* n 10) (digit-val ch)))
           (cons n pos))))
   (defun read-int (source pos)
     (let* ((neg nil) (start pos) (ch (char-at source pos)))
       (cond ((= ch #x2D) (setf neg t) (setf start (+ pos 1)))
             ((= ch #x2B) (setf start (+ pos 1))))
       (let ((result (read-digits source start 0)))
         (let ((val (car result)) (end (cdr result)))
           (cons (if neg (- 0 val) val) end)))))
   (- 0 (car (read-int \"-17\" 0)))"
  17)

;;; Hex parsing

(test-libsystem-code "read-hex-simple"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun hex-digit? (ch) (or (digit? ch) (and (>= ch #x41) (<= ch #x46)) (and (>= ch #x61) (<= ch #x66))))
   (defun hex-val (ch) (cond ((digit? ch) (- ch #x30)) ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) #xA)) ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) #xA)) (t #x0)))
   (defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (defun read-hex-digits (source pos n)
     (let ((ch (char-at source pos)))
       (if (hex-digit? ch)
           (read-hex-digits source (+ pos 1) (+ (* n 16) (hex-val ch)))
           (cons n pos))))
   (car (read-hex-digits \"FF\" 0 0))"
  255)

(test-libsystem-code "read-hex-mixed"
  "(defun digit? (ch) (and (>= ch #x30) (<= ch #x39)))
   (defun hex-digit? (ch) (or (digit? ch) (and (>= ch #x41) (<= ch #x46)) (and (>= ch #x61) (<= ch #x66))))
   (defun hex-val (ch) (cond ((digit? ch) (- ch #x30)) ((and (>= ch #x41) (<= ch #x46)) (+ (- ch #x41) #xA)) ((and (>= ch #x61) (<= ch #x66)) (+ (- ch #x61) #xA)) (t #x0)))
   (defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (defun read-hex-digits (source pos n)
     (let ((ch (char-at source pos)))
       (if (hex-digit? ch)
           (read-hex-digits source (+ pos 1) (+ (* n 16) (hex-val ch)))
           (cons n pos))))
   (car (read-hex-digits \"2A\" 0 0))"
  42)

;;; Skip whitespace and comments

(test-libsystem-code "skip-ws-spaces"
  "(defun whitespace? (ch) (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))
   (defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (defun skip-ws (source pos)
     (let ((ch (char-at source pos)))
       (if (whitespace? ch)
           (skip-ws source (+ pos 1))
           pos)))
   (skip-ws \"   42\" 0)"
  3)

(test-libsystem-code "skip-ws-none"
  "(defun whitespace? (ch) (or (= ch #x20) (= ch #x09) (= ch #x0A) (= ch #x0D)))
   (defun char-at (source pos) (if (< pos (string-length source)) (string-ref source pos) #x0))
   (defun skip-ws (source pos)
     (let ((ch (char-at source pos)))
       (if (whitespace? ch)
           (skip-ws source (+ pos 1))
           pos)))
   (skip-ws \"42\" 0)"
  0)

;;; String building via chars-to-string
;;; Using simple loop instead of nth for now

(test-libsystem-code "chars-to-string-single"
  "(defun chars-to-string (chars)
     (let* ((len (length chars))
            (vec (make-vector len)))
       (labels ((fill (i lst)
                  (if (null lst)
                      vec
                      (progn
                        (vector-set vec i (car lst))
                        (fill (+ i 1) (cdr lst))))))
         (fill 0 chars))
       (make-string-from-vector vec)))
   (string-length (chars-to-string (cons 65 nil)))"
  1)

(test-libsystem-code "chars-to-string-multi"
  "(defun chars-to-string (chars)
     (let* ((len (length chars))
            (vec (make-vector len)))
       (labels ((fill (i lst)
                  (if (null lst)
                      vec
                      (progn
                        (vector-set vec i (car lst))
                        (fill (+ i 1) (cdr lst))))))
         (fill 0 chars))
       (make-string-from-vector vec)))
   (string-ref (chars-to-string (cons 65 (cons 66 (cons 67 nil)))) 1)"
  66)

;;; Report results
(format t "~%Total: ~A passed, ~A failed~%" *pass-count* *fail-count*)
(sb-ext:exit :code (if (zerop *fail-count*) 0 1))
