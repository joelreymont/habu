;;; Test symbol interning to verify eq works

;; Simple test that doesn't use print
(defun test-intern-eq ()
  "Test if interning the same name twice returns the same symbol object"
  (let ((s1 (intern "FOO"))
        (s2 (intern "FOO")))
    (if (eq s1 s2)
        (sys-write-string "SUCCESS: (eq (intern \"FOO\") (intern \"FOO\")) = t\n")
        (progn
          (sys-write-string "FAILURE: (eq (intern \"FOO\") (intern \"FOO\")) = nil\n")
          (sys-write-string "Names equal? ")
          (if (string-equal (symbol-name s1) (symbol-name s2))
              (sys-write-string "yes\n")
              (sys-write-string "no\n"))))))

(defun test-read-symbols-eq ()
  "Test if reading the same symbol name twice produces eq symbols"
  (let* ((code "(list 'QUOTE 'QUOTE)")
         (expr (car (read-all code)))
         (sym1 (car (cdr expr)))
         (sym2 (car (cdr (cdr expr)))))
    (if (eq sym1 sym2)
        (sys-write-string "SUCCESS: symbols read from same name are eq\n")
        (progn
          (sys-write-string "FAILURE: symbols read from same name are not eq\n")
          (sys-write-string "Names equal? ")
          (if (string-equal (symbol-name sym1) (symbol-name sym2))
              (sys-write-string "yes\n")
              (sys-write-string "no\n"))))))

(defun test-op-symbols ()
  "Test if operator symbols initialized in init-builtin-dispatch are eq to freshly interned ones"
  (let ((quote1 *op-quote*)
        (quote2 (intern "QUOTE")))
    (if (eq quote1 quote2)
        (sys-write-string "SUCCESS: *op-quote* is eq to (intern \"QUOTE\")\n")
        (progn
          (sys-write-string "FAILURE: *op-quote* is NOT eq to (intern \"QUOTE\")\n")
          (sys-write-string "Names equal? ")
          (if (string-equal (symbol-name quote1) (symbol-name quote2))
              (sys-write-string "yes\n")
              (sys-write-string "no\n"))))))

(sys-write-string "=== Testing Symbol Interning ===\n")
(test-intern-eq)
(test-read-symbols-eq)
(test-op-symbols)
(sys-write-string "=== Tests Complete ===\n")
