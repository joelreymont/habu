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
      (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" "-f" output-path)
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

;;; ============================================================
;;; Full reader tests - compile entire reader into native executable
;;; ============================================================

(defun get-reader-source ()
  (with-open-file (s "common/reader.lisp")
    (let ((content (make-string (file-length s))))
      (read-sequence content s)
      content)))

;; Full reader: parse integer
(test-libsystem-code "full-read-int"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(read-from-string \"42\"))"
          (get-reader-source))
  42)

;; Full reader: parse negative integer
(test-libsystem-code "full-read-neg"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(let ((n (read-from-string \"-10\"))) (+ 52 n)))"
          (get-reader-source))
  42)

;; Full reader: parse hex number
(test-libsystem-code "full-read-hex"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(read-from-string \"#x2A\"))"
          (get-reader-source))
  42)

;; Full reader: parse symbol and compare
;; Symbol test: verify symbol is parsed (check symbolp, not eq with compile-time symbol
;; because runtime intern creates new symbol IDs different from compile-time symbols)
(test-libsystem-code "full-read-symbol"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(if (symbolp (read-from-string \"foo\")) 42 0))"
          (get-reader-source))
  42)

;; Full reader: parse list and get car
(test-libsystem-code "full-read-list-car"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(car (read-from-string \"(42 1 2)\")))"
          (get-reader-source))
  42)

;; Full reader: parse list and get cadr
(test-libsystem-code "full-read-list-cadr"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(cadr (read-from-string \"(1 42 3)\")))"
          (get-reader-source))
  42)

;; Full reader: parse quoted form
(test-libsystem-code "full-read-quote"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(let ((x (read-from-string \"'(1 2 3)\"))) (if (eq (car x) 'QUOTE) 42 0)))"
          (get-reader-source))
  42)

;; Full reader: parse string and get length
(test-libsystem-code "full-read-string-len"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(+ 40 (string-length (read-from-string \"\\\"ab\\\"\"))))"
          (get-reader-source))
  42)

;; Full reader: read multiple forms
(test-libsystem-code "full-read-multiple"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(let ((forms (read-all-from-string \"10 20 12\"))) (+ (car forms) (cadr forms) (caddr forms))))"
          (get-reader-source))
  42)

;; Full reader: parse expression and compute
(test-libsystem-code "full-read-compute"
  (format nil "(progn (sys-write 1 \"\" 0)~%~A~%(let ((expr (read-from-string \"(+ 20 22)\"))) (+ (cadr expr) (caddr expr))))"
          (get-reader-source))
  42)

;;; Report results
(format t "~%Total: ~A passed, ~A failed~%" *pass-count* *fail-count*)
(sb-ext:exit :code (if (zerop *fail-count*) 0 1))
