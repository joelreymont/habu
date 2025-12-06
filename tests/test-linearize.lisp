;;;; Linearization Pass Tests
;;;; Tests the Tree IR -> Linear IR conversion
;;;; Loaded via ASDF as part of habu/tests system.

(in-package :habu-test)

;;; ============================================================
;;; Helper for unit tests (not compilation tests)
;;; ============================================================

(defun unit-test (name condition)
  "Assert a unit test condition - not compilation-based."
  (if condition
      (progn
        (when *test-verbose* (format t "[PASS] ~A~%" name))
        (incf *pass-count*)
        t)
      (progn
        (format t "[FAIL] ~A~%" name)
        (incf *fail-count*)
        nil)))

(defun run-linearize-tests ()
  "Run all linearization pass tests."
  (define-test-suite "Linearization Pass Tests"

    ;; ========================================
    ;; Leaf Node Tests
    ;; ========================================
    (format t "~%--- Leaf Nodes ---~%")

    ;; Test literal
    (let ((lin (habu::linearize '(habu::lit 42))))
      (unit-test "literal produces load-lit"
                 (eq (caar lin) 'habu::load-lit))
      (unit-test "literal value preserved"
                 (eql (caddar lin) 42))
      (unit-test "literal ends with result"
                 (eq (caadr lin) 'habu::result)))

    ;; Test nil
    (let ((lin (habu::linearize '(habu::nil-ir))))
      (unit-test "nil-ir produces load-nil"
                 (eq (caar lin) 'habu::load-nil)))

    ;; Test variable reference
    (let ((lin (habu::linearize '(habu::var 3))))
      (unit-test "var produces load-var"
                 (eq (caar lin) 'habu::load-var))
      (unit-test "var offset preserved"
                 (eql (caddar lin) 3)))

    ;; Test symbol literal
    (let ((lin (habu::linearize '(habu::sym-lit foo))))
      (unit-test "sym-lit produces load-sym"
                 (eq (caar lin) 'habu::load-sym))
      (unit-test "sym-lit name preserved"
                 (eq (caddar lin) 'foo)))

    ;; Test string literal
    (let ((lin (habu::linearize '(habu::str-lit "hello"))))
      (unit-test "str-lit produces load-str"
                 (eq (caar lin) 'habu::load-str))
      (unit-test "str-lit value preserved"
                 (string= (caddar lin) "hello")))

    ;; ========================================
    ;; Binary Operation Tests
    ;; ========================================
    (format t "~%--- Binary Operations ---~%")

    ;; Test add
    (let ((lin (habu::linearize '(habu::add (habu::lit 10) (habu::lit 20)))))
      (unit-test "add: linearizes both operands first"
                 (eq (caar lin) 'habu::load-lit))
      (unit-test "add: emits add instruction"
                 (member 'habu::add (mapcar #'car lin)))
      (unit-test "add: correct instruction count"
                 (= (length lin) 4)))  ; load, load, add, result

    ;; Test sub
    (let ((lin (habu::linearize '(habu::sub (habu::lit 50) (habu::lit 30)))))
      (unit-test "sub: emits sub instruction"
                 (member 'habu::sub (mapcar #'car lin))))

    ;; Test mul
    (let ((lin (habu::linearize '(habu::mul (habu::lit 6) (habu::lit 7)))))
      (unit-test "mul: emits mul instruction"
                 (member 'habu::mul (mapcar #'car lin))))

    ;; Test div
    (let ((lin (habu::linearize '(habu::div (habu::lit 100) (habu::lit 5)))))
      (unit-test "div: emits div instruction"
                 (member 'habu::div (mapcar #'car lin))))

    ;; Test mod
    (let ((lin (habu::linearize '(habu::mod (habu::lit 17) (habu::lit 5)))))
      (unit-test "mod: emits mod instruction"
                 (member 'habu::mod (mapcar #'car lin))))

    ;; ========================================
    ;; Comparison Tests
    ;; ========================================
    (format t "~%--- Comparisons ---~%")

    (let ((lin (habu::linearize '(habu::cmp-eq (habu::lit 5) (habu::lit 5)))))
      (unit-test "cmp-eq: emits cmp-eq instruction"
                 (member 'habu::cmp-eq (mapcar #'car lin))))

    (let ((lin (habu::linearize '(habu::cmp-lt (habu::lit 3) (habu::lit 5)))))
      (unit-test "cmp-lt: emits cmp-lt instruction"
                 (member 'habu::cmp-lt (mapcar #'car lin))))

    (let ((lin (habu::linearize '(habu::cmp-gt (habu::lit 10) (habu::lit 5)))))
      (unit-test "cmp-gt: emits cmp-gt instruction"
                 (member 'habu::cmp-gt (mapcar #'car lin))))

    (let ((lin (habu::linearize '(habu::cmp-le (habu::lit 5) (habu::lit 5)))))
      (unit-test "cmp-le: emits cmp-le instruction"
                 (member 'habu::cmp-le (mapcar #'car lin))))

    (let ((lin (habu::linearize '(habu::cmp-ge (habu::lit 10) (habu::lit 5)))))
      (unit-test "cmp-ge: emits cmp-ge instruction"
                 (member 'habu::cmp-ge (mapcar #'car lin))))

    ;; ========================================
    ;; List Operation Tests
    ;; ========================================
    (format t "~%--- List Operations ---~%")

    (let ((lin (habu::linearize '(habu::cons-ir (habu::lit 1) (habu::lit 2)))))
      (unit-test "cons-ir: emits cons instruction"
                 (member 'habu::cons (mapcar #'car lin))))

    (let ((lin (habu::linearize '(habu::car-ir (habu::var 0)))))
      (unit-test "car-ir: emits car instruction"
                 (member 'habu::car (mapcar #'car lin))))

    (let ((lin (habu::linearize '(habu::cdr-ir (habu::var 0)))))
      (unit-test "cdr-ir: emits cdr instruction"
                 (member 'habu::cdr (mapcar #'car lin))))

    ;; ========================================
    ;; Control Flow Tests
    ;; ========================================
    (format t "~%--- Control Flow ---~%")

    ;; Test if
    (let ((lin (habu::linearize '(habu::if-ir (habu::lit 1) (habu::lit 10) (habu::lit 20)))))
      (unit-test "if-ir: emits jump-if-nil"
                 (member 'habu::jump-if-nil (mapcar #'car lin)))
      (unit-test "if-ir: emits labels"
                 (member 'habu::label (mapcar #'car lin)))
      (unit-test "if-ir: emits jump"
                 (member 'habu::jump (mapcar #'car lin)))
      (unit-test "if-ir: emits move for branch results"
                 (member 'habu::move (mapcar #'car lin))))

    ;; Test while
    (let ((lin (habu::linearize '(habu::while-ir (habu::lit 0) (habu::lit 99)))))
      (unit-test "while-ir: has loop label"
                 (member 'habu::label (mapcar #'car lin)))
      (unit-test "while-ir: has jump-if-nil for exit"
                 (member 'habu::jump-if-nil (mapcar #'car lin)))
      (unit-test "while-ir: has unconditional jump for loop"
                 (member 'habu::jump (mapcar #'car lin))))

    ;; Test progn
    (let ((lin (habu::linearize '(habu::progn-ir ((habu::lit 1) (habu::lit 2) (habu::lit 3))))))
      (unit-test "progn-ir: linearizes all forms"
                 (= (count 'habu::load-lit lin :key #'car) 3))
      (unit-test "progn-ir: result is last form's temp"
                 (let ((result-instr (car (last lin))))
                   (eq (car result-instr) 'habu::result))))

    ;; Empty progn
    (let ((lin (habu::linearize '(habu::progn-ir nil))))
      (unit-test "empty progn: produces nil"
                 (member 'habu::load-nil (mapcar #'car lin))))

    ;; ========================================
    ;; Binding Tests
    ;; ========================================
    (format t "~%--- Bindings ---~%")

    ;; Test let
    (let ((lin (habu::linearize '(habu::let-ir ((habu::lit 42)) (habu::var 0) 1 0))))
      (unit-test "let-ir: emits bind instruction"
                 (member 'habu::bind (mapcar #'car lin)))
      (unit-test "let-ir: emits store-binding"
                 (member 'habu::store-binding (mapcar #'car lin)))
      (unit-test "let-ir: emits unbind instruction"
                 (member 'habu::unbind (mapcar #'car lin))))

    ;; Test setq
    (let ((lin (habu::linearize '(habu::setq-ir 0 (habu::lit 100)))))
      (unit-test "setq-ir: emits setq instruction"
                 (member 'habu::setq (mapcar #'car lin))))

    ;; ========================================
    ;; Function Call Tests
    ;; ========================================
    (format t "~%--- Function Calls ---~%")

    ;; Test call
    (let ((lin (habu::linearize '(habu::call-fn foo ((habu::lit 1) (habu::lit 2))))))
      (unit-test "call-fn: emits call instruction"
                 (member 'habu::call (mapcar #'car lin)))
      (unit-test "call-fn: preserves function name"
                 (let ((call-instr (find 'habu::call lin :key #'car)))
                   (eq (caddr call-instr) 'foo))))

    ;; Test funcall
    (let ((lin (habu::linearize '(habu::funcall-ir (habu::var 0) ((habu::lit 1))))))
      (unit-test "funcall-ir: emits funcall instruction"
                 (member 'funcall (mapcar #'car lin))))

    ;; ========================================
    ;; Nested Expression Tests
    ;; ========================================
    (format t "~%--- Nested Expressions ---~%")

    ;; Deeply nested arithmetic
    (let ((lin (habu::linearize '(habu::add (habu::mul (habu::lit 2) (habu::lit 3))
                                            (habu::sub (habu::lit 10) (habu::lit 5))))))
      (unit-test "nested: flattens to correct count"
                 ;; 4 load-lit + 1 mul + 1 sub + 1 add + 1 result = 8
                 (= (length lin) 8))
      (unit-test "nested: operations in correct order (mul before add)"
                 (< (position 'habu::mul lin :key #'car)
                    (position 'habu::add lin :key #'car)))
      (unit-test "nested: operations in correct order (sub before add)"
                 (< (position 'habu::sub lin :key #'car)
                    (position 'habu::add lin :key #'car))))

    ;; If inside arithmetic
    (let ((lin (habu::linearize '(habu::add (habu::lit 1)
                                            (habu::if-ir (habu::lit 0) (habu::lit 10) (habu::lit 20))))))
      (unit-test "if in arithmetic: produces valid linear IR"
                 (and (member 'habu::add (mapcar #'car lin))
                      (member 'habu::jump-if-nil (mapcar #'car lin)))))

    ;; ========================================
    ;; Temp Slot Assignment Tests
    ;; ========================================
    (format t "~%--- Temp Slot Assignment ---~%")

    (let ((lin (habu::linearize '(habu::add (habu::lit 1) (habu::lit 2)))))
      (let* ((load1 (first lin))
             (load2 (second lin))
             (add-instr (third lin)))
        (unit-test "temps: first operand gets temp 0"
                   (= (cadr load1) 0))
        (unit-test "temps: second operand gets temp 1"
                   (= (cadr load2) 1))
        (unit-test "temps: result gets temp 2"
                   (= (cadr add-instr) 2))
        (unit-test "temps: add uses correct source temps"
                   (and (= (caddr add-instr) 0)
                        (= (cadddr add-instr) 1)))))))

;; Run tests when loaded
(run-linearize-tests)
