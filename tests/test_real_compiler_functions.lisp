#!/usr/bin/env sbcl --script
;;; Test Real Compiler Functions
;;;
;;; This test compiles and runs actual functions from the Habu compiler
;;; to verify they work correctly when compiled to ARM64.

(load "run-habu.lisp")

(format t "~%=== TESTING REAL COMPILER FUNCTIONS ===~%~%")

;; Test 1: has-tag? from common/utils.lisp
(format t "Test 1: has-tag? function~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun has-tag? (ir tag)
                   (and (consp ir) (eq (car ir) tag)))

                 ;; Test cases
                 (let ((ir1 (list 'lit #x42))
                       (ir2 (list 'var #x0))
                       (ir3 #x5))
                   (+ (if (has-tag? ir1 'lit) #x1 #x0)      ; should be 1
                      (if (has-tag? ir1 'var) #x10 #x0)    ; should be 0
                      (if (has-tag? ir2 'var) #x100 #x0)   ; should be 256
                      (if (has-tag? ir3 'lit) #x1000 #x0)))))))  ; should be 0
  (format t "  Result: ~A (expected 257 = 0x101)~%" result)
  (unless (= result #x101)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 2: env-lookup from common/utils.lisp
(format t "~%Test 2: env-lookup function~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun env-lookup (sym env)
                   (if (nil? env)
                       nil
                       (if (eq (car (car env)) sym)
                           (cdr (car env))
                           (env-lookup sym (cdr env)))))

                 ;; Build environment: ((x . 0) (y . 1) (z . 2))
                 (let ((env (list (cons 'x #x0)
                                  (cons 'y #x1)
                                  (cons 'z #x2))))
                   ;; Lookup y, should return 1
                   (let ((result (env-lookup 'y env)))
                     (if (numberp result) result #xFF)))))))
  (format t "  Result: ~A (expected 1)~%" result)
  (unless (= result #x1)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 3: op= from common/utils.lisp (package-agnostic comparison)
;; Now implemented! string= and symbol-name are available
(format t "~%Test 3: op= function~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun my-op= (sym name-str)
                   ;; Package-agnostic symbol name comparison
                   (and (symbolp sym)
                        (string= (symbol-name sym) name-str)))
                 ;; Test: 'foo should match "FOO" (symbols are uppercase)
                 (+ (if (my-op= 'foo (symbol-name 'foo)) #x1 #x0)      ; should be 1
                    (if (my-op= 'bar (symbol-name 'baz)) #x10 #x0)     ; should be 0
                    (if (my-op= 'hello (symbol-name 'hello)) #x100 #x0)))))) ; should be 256
  (format t "  Result: ~A (expected 257 = 0x101)~%" result)
  (unless (= result #x101)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 4: remove-duplicates from common/utils.lisp
;; NOTE: Use "my-remove-dups" to avoid shadowing built-in remove-duplicates
(format t "~%Test 4: remove-duplicates function~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '(;; Helper defined first
                 (defun rd-iter (remaining seen)
                   (if (nil? remaining)
                       (reverse seen)
                       (let ((el (car remaining)))
                         (if (member el seen)
                             (rd-iter (cdr remaining) seen)
                             (rd-iter (cdr remaining) (cons el seen))))))

                 ;; Wrapper (avoid name 'remove-duplicates' - it shadows built-in)
                 (defun my-remove-dups (lst)
                   (rd-iter lst nil))

                 ;; Test: (my-remove-dups '(1 2 1 3 2 4)) -> (1 2 3 4)
                 (length (my-remove-dups (list #x1 #x2 #x1 #x3 #x2 #x4)))))))
  (format t "  Result: ~A (expected 4)~%" result)
  (unless (= result #x4)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 5: collect-var-offsets pattern (simplified)
(format t "~%Test 5: collect-var-offsets pattern~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun has-tag? (ir tag)
                   (and (consp ir) (eq (car ir) tag)))

                 (defun collect-offsets (ir)
                   (if (nil? ir)
                       nil
                       (if (has-tag? ir 'var)
                           (list (cadr ir))
                           (if (consp ir)
                               (append (collect-offsets (car ir))
                                       (collect-offsets (cdr ir)))
                               nil))))

                 ;; Test IR: (add (var 0) (add (var 1) (var 0)))
                 ;; Should collect offsets: (0 1 0)
                 (let ((ir (list 'add
                                 (list 'var #x0)
                                 (list 'add
                                       (list 'var #x1)
                                       (list 'var #x0)))))
                   (length (collect-offsets ir)))))))
  (format t "  Result: ~A (expected 3)~%" result)
  (unless (= result #x3)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 6: Simple IR compilation pattern
;; NOTE: Use single recursive function to avoid forward reference issues
(format t "~%Test 6: IR compilation pattern (compile-expr style)~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun has-tag? (ir tag)
                   (and (consp ir) (eq (car ir) tag)))

                 ;; Single compile function handles both literals and compounds
                 (defun compile-simple (expr env)
                   (cond
                     ((numberp expr) (list 'lit expr))
                     ((consp expr)
                      (let ((op (car expr)))
                        (cond
                          ((eq op '+)
                           (list 'add
                                 (compile-simple (cadr expr) env)
                                 (compile-simple (caddr expr) env)))
                          ((eq op '-)
                           (list 'sub
                                 (compile-simple (cadr expr) env)
                                 (compile-simple (caddr expr) env)))
                          ((eq op '*)
                           (list 'mul
                                 (compile-simple (cadr expr) env)
                                 (compile-simple (caddr expr) env)))
                          (t (list 'lit #x0)))))
                     (t (list 'lit #x0))))

                 ;; Compile (+ 10 (* 3 4)) -> (add (lit 10) (mul (lit 3) (lit 4)))
                 ;; Then check structure
                 (let ((ir (compile-simple (list '+ #xa (list '* #x3 #x4)) nil)))
                   (if (has-tag? ir 'add)
                       (if (has-tag? (caddr ir) 'mul)
                           #x1
                           #x0)
                       #x0))))))
  (format t "  Result: ~A (expected 1)~%" result)
  (unless (= result #x1)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 7: env-extend pattern (simplified)
(format t "~%Test 7: env-extend pattern~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun env-extend-simple (bindings env)
                   ;; Simple version: just prepend bindings with sequential offsets
                   (labels ((add-bindings (bs offset acc)
                              (if (nil? bs)
                                  acc
                                  (add-bindings (cdr bs)
                                                (+ offset #x1)
                                                (cons (cons (car bs) offset) acc)))))
                     (let ((start-offset (if env
                                             (+ (cdr (car env)) #x1)
                                             #x0)))
                       (append (reverse (add-bindings bindings start-offset nil)) env))))

                 ;; Test: extend nil with (a b c)
                 ;; Should produce ((a . 0) (b . 1) (c . 2))
                 (let ((new-env (env-extend-simple (list 'a 'b 'c) nil)))
                   (+ (cdr (car new-env))           ; a -> 0
                      (cdr (cadr new-env))          ; b -> 1
                      (cdr (caddr new-env))))))))   ; c -> 2 = 3
  (format t "  Result: ~A (expected 3)~%" result)
  (unless (= result #x3)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 8: mapcar with compiler-like transformation
(format t "~%Test 8: mapcar in compiler context~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun compile-lit (n)
                   (list 'lit n))

                 ;; Compile a list of numbers to IR literals
                 ;; Now using (function compile-lit) directly!
                 (let ((irs (mapcar (function compile-lit) (list #x1 #x2 #x3 #x4 #x5))))
                   ;; Sum up all the literal values
                   (reduce (lambda (acc ir) (+ acc (cadr ir)))
                           irs
                           #x0))))))
  (format t "  Result: ~A (expected 15)~%" result)
  (unless (= result #xf)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 9: Recursive IR traversal (codegen pattern)
(format t "~%Test 9: Recursive IR evaluation (codegen pattern)~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun eval-simple-ir (ir)
                   (cond
                     ((eq (car ir) 'lit) (cadr ir))
                     ((eq (car ir) 'add)
                      (+ (eval-simple-ir (cadr ir))
                         (eval-simple-ir (caddr ir))))
                     ((eq (car ir) 'mul)
                      (* (eval-simple-ir (cadr ir))
                         (eval-simple-ir (caddr ir))))
                     (t #x0)))

                 ;; Evaluate: (add (lit 10) (mul (lit 3) (lit 4)))
                 ;; = 10 + (3 * 4) = 10 + 12 = 22
                 (eval-simple-ir
                  (list 'add
                        (list 'lit #xa)
                        (list 'mul
                              (list 'lit #x3)
                              (list 'lit #x4))))))))
  (format t "  Result: ~A (expected 22)~%" result)
  (unless (= result #x16)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

;; Test 10: Full compile + eval round trip
(format t "~%Test 10: Full compile + eval round trip~%")
(let ((result (habu-sbcl:compile-and-run-forms
               '((defun compile-simple (expr)
                   (cond
                     ((numberp expr) (list 'lit expr))
                     ((eq (car expr) '+)
                      (list 'add
                            (compile-simple (cadr expr))
                            (compile-simple (caddr expr))))
                     ((eq (car expr) '*)
                      (list 'mul
                            (compile-simple (cadr expr))
                            (compile-simple (caddr expr))))
                     (t (list 'lit #x0))))

                 (defun eval-ir (ir)
                   (cond
                     ((eq (car ir) 'lit) (cadr ir))
                     ((eq (car ir) 'add)
                      (+ (eval-ir (cadr ir)) (eval-ir (caddr ir))))
                     ((eq (car ir) 'mul)
                      (* (eval-ir (cadr ir)) (eval-ir (caddr ir))))
                     (t #x0)))

                 (defun run (expr)
                   (eval-ir (compile-simple expr)))

                 ;; Compile and run: (* (+ 2 3) (+ 4 1))
                 ;; = (2+3) * (4+1) = 5 * 5 = 25
                 (run (list '* (list '+ #x2 #x3) (list '+ #x4 #x1)))))))
  (format t "  Result: ~A (expected 25)~%" result)
  (unless (= result #x19)
    (format t "  *** FAILED~%")
    (sb-ext:quit :unix-status 1)))

(format t "~%=== ALL REAL COMPILER FUNCTION TESTS PASSED ===~%")
(format t "~%Successfully compiled and ran:~%")
(format t "  - has-tag? (IR tag checking)~%")
(format t "  - env-lookup (environment lookup)~%")
(format t "  - op= (symbol name comparison)~%")
(format t "  - remove-duplicates (list processing)~%")
(format t "  - collect-var-offsets (IR traversal)~%")
(format t "  - compile-arith (IR generation)~%")
(format t "  - env-extend (environment building)~%")
(format t "  - mapcar with compiler transforms~%")
(format t "  - Recursive IR evaluation~%")
(format t "  - Full compile + eval round trip~%")

(sb-ext:quit :unix-status 0)
