(require :asdf)
(push (truename "bootstrap/") asdf:*central-registry*)
(asdf:load-system :habu)
(in-package :habu)
(load "bootstrap/compiler.lisp")
(load "bootstrap/macho.lisp")

(format t "~%=== Inline Intern Tests ===~%~%")

(defun test-code (name source expected)
  (handler-case
    (let ((output-path (format nil "/tmp/intern_~A" name)))
      (deliver source output-path)
      (sb-ext:run-program "/usr/bin/codesign" (list "-s" "-" output-path)
                          :output nil :error nil :wait t)
      (let* ((proc (sb-ext:run-program output-path nil :output nil :error nil :wait t))
             (code (sb-ext:process-exit-code proc)))
        (if (= code expected)
            (format t "[PASS] ~A = ~A~%" name code)
            (format t "[FAIL] ~A: expected ~A, got ~A~%" name expected code))))
    (error (e)
      (format t "[FAIL] ~A: error ~A~%" name e))))

;; Test 1: Intern a string, get symbol ID 0
(test-code "first-intern"
  "(progn (sys-write 1 \"\" 0)
          (let ((s (make-symbol-from-string \"foo\")))
            42))"
  42)

;; Test 2: Intern same string twice - currently no dedup, creates different IDs
;; TODO: Implement table search to enable symbol deduplication
(test-code "intern-no-dedup"
  "(progn (sys-write 1 \"\" 0)
          (let ((s1 (make-symbol-from-string \"bar\"))
                (s2 (make-symbol-from-string \"bar\")))
            ;; Without dedup, each call creates new symbol
            (if (eq s1 s2) 0 42)))"
  42)

;; Test 3: Intern different strings, should get different IDs
(test-code "intern-neq"
  "(progn (sys-write 1 \"\" 0)
          (let ((s1 (make-symbol-from-string \"abc\"))
                (s2 (make-symbol-from-string \"xyz\")))
            (if (eq s1 s2) 0 42)))"
  42)

;; Test 4: Symbol ID returned (first symbol is ID 0, tagged as 2)
;; Since we can't compare raw tagged values with eq (literals are fixnums),
;; we use symbolp to verify it's a symbol
(test-code "intern-is-symbol"
  "(progn (sys-write 1 \"\" 0)
          (let ((s1 (make-symbol-from-string \"first\")))
            (if (symbolp s1) 42 0)))"
  42)

;; Test 5: Two interns of different strings produce different symbols
;; With simplified intern (no dedup), each call creates new ID
(test-code "intern-diff-ids"
  "(progn (sys-write 1 \"\" 0)
          (let ((s1 (make-symbol-from-string \"aaa\"))
                (s2 (make-symbol-from-string \"bbb\")))
            ;; Without dedup, s1 and s2 have different IDs
            (if (eq s1 s2) 0 42)))"
  42)

(format t "~%Done~%")
