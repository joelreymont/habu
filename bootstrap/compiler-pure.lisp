;;; Pure Habu Compiler - Uses ONLY Habu primitives (no SBCL dependencies)
;;; No multiple-value-bind, no values, no loop, no format
;;; This can be compiled to native and run without SBCL

(in-package :habu)

;;; ============================================================
;;; Core Helpers (Pure Habu)
;;; ============================================================

(defun pure-append (lst1 lst2)
  "Append two lists without using CL append"
  (labels ((append-iter (l acc)
             (if (null l)
                 acc
                 (append-iter (cdr l) (cons (car l) acc)))))
    (append-iter (reverse lst1) lst2)))

(defun pure-reverse (lst)
  "Reverse a list"
  (labels ((rev-iter (l acc)
             (if (null l)
                 acc
                 (rev-iter (cdr l) (cons (car l) acc)))))
    (rev-iter lst nil)))

(defun pure-length (lst)
  "List length"
  (labels ((len-iter (l n)
             (if (null l)
                 n
                 (len-iter (cdr l) (+ n 1)))))
    (len-iter lst 0)))

(defun pure-nth (n lst)
  "Get nth element"
  (if (= n 0)
      (car lst)
      (pure-nth (- n 1) (cdr lst))))

;;; ============================================================
;;; Pure Compiler Core
;;; ============================================================

(defun pure-compile-lit (val)
  "Compile literal to IR"
  (list 'lit val))

(defun pure-compile-var (sym env)
  "Compile variable reference"
  (let ((offset (pure-env-lookup sym env)))
    (if offset
        (list 'var offset)
        (list 'lit 0))))  ;; Unknown var = 0

(defun pure-env-lookup (sym env)
  "Look up symbol in environment, return offset or nil"
  (labels ((search-env (e offset)
             (if (null e)
                 nil
                 (if (eq (car e) sym)
                     offset
                     (search-env (cdr e) (+ offset 1))))))
    (search-env env 0)))

(defun pure-compile-if (expr env)
  "Compile (if test then else) to IR"
  (let ((test (pure-compile-expr (nth 1 expr) env))
        (then (pure-compile-expr (nth 2 expr) env))
        (else (pure-compile-expr (nth 3 expr) env)))
    (list 'if-ir test then else)))

(defun pure-compile-expr (expr env)
  "Compile expression to IR - pure Habu version"
  (cond
    ;; Literal numbers
    ((numberp expr) (pure-compile-lit expr))
    ;; Symbols
    ((symbolp expr) (pure-compile-var expr env))
    ;; Not a list - treat as lit 0
    ((not (consp expr)) (pure-compile-lit 0))
    ;; Lists: check operator
    (t
     (let ((op (car expr)))
       (cond
         ;; (if test then else)
         ((eq op 'if)
          (pure-compile-if expr env))
         ;; (+ a b)
         ((eq op '+)
          (list 'add (pure-compile-expr (nth 1 expr) env)
                     (pure-compile-expr (nth 2 expr) env)))
         ;; (* a b)
         ((eq op '*)
          (list 'mul (pure-compile-expr (nth 1 expr) env)
                     (pure-compile-expr (nth 2 expr) env)))
         ;; (- a b)
         ((eq op '-)
          (list 'sub (pure-compile-expr (nth 1 expr) env)
                     (pure-compile-expr (nth 2 expr) env)))
         ;; (= a b)
         ((eq op '=)
          (list 'cmp-eq (pure-compile-expr (nth 1 expr) env)
                        (pure-compile-expr (nth 2 expr) env)))
         ;; Default: unknown, compile to lit 0
         (t (pure-compile-lit 0)))))))

;;; Export pure compiler
(export '(pure-compile-expr pure-append pure-reverse pure-length) :habu)
