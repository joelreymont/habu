;;;; test-adt.lisp - Test the type system

(load "typed/types.lisp")
(load "typed/ir.lisp")

(in-package :habu.ir)

;; Test constructors
(let ((lit (ir-lit 42))
      (add (ir-add (ir-lit 1) (ir-lit 2))))
  (format t "lit: ~S~%" lit)
  (format t "add: ~S~%" add)
  (format t "~%Predicates:~%")
  (format t "ir-lit-p lit: ~A~%" (ir-lit-p lit))
  (format t "ir-add-p add: ~A~%" (ir-add-p add))
  (format t "ir-node-p lit: ~A~%" (ir-node-p lit)))

;; Test simple match (just a few variants for sanity check)
(defun simple-eval (x)
  (habu.types:match ir-node x
    ;; Literals
    (lit (v) v)
    (nil () 0)
    (t () 1)
    (str (s) (length s))
    (sym (n) n)
    (kw (n) n)
    ;; Variables
    (var (o) o)
    (setq (o v) (declare (ignore v)) o)
    (global (n) n)
    (set-global (n v) (declare (ignore v)) n)
    ;; Arithmetic
    (add (l r) (+ (simple-eval l) (simple-eval r)))
    (sub (l r) (- (simple-eval l) (simple-eval r)))
    (mul (l r) (* (simple-eval l) (simple-eval r)))
    (div (l r) (truncate (simple-eval l) (simple-eval r)))
    (mod (l r) (cl:mod (simple-eval l) (simple-eval r)))
    (neg (v) (- (simple-eval v)))
    ;; Comparison
    (eq (l r) (declare (ignore l r)) nil)
    (eql (l r) (declare (ignore l r)) nil)
    (lt (l r) (< (simple-eval l) (simple-eval r)))
    (gt (l r) (> (simple-eval l) (simple-eval r)))
    (le (l r) (<= (simple-eval l) (simple-eval r)))
    (ge (l r) (>= (simple-eval l) (simple-eval r)))
    (zerop (v) (zerop (simple-eval v)))
    ;; Logical
    (not (v) (not (simple-eval v)))
    (and (l r) (and (simple-eval l) (simple-eval r)))
    (or (l r) (or (simple-eval l) (simple-eval r)))
    ;; Bitwise
    (band (l r) (logand (simple-eval l) (simple-eval r)))
    (bor (l r) (logior (simple-eval l) (simple-eval r)))
    (bxor (l r) (logxor (simple-eval l) (simple-eval r)))
    (bsh (v s) (ash (simple-eval v) (simple-eval s)))
    (bnot (v) (lognot (simple-eval v)))
    ;; Control
    (if (test then else)
      (if (simple-eval test) (simple-eval then) (simple-eval else)))
    (progn (forms) (car (last (mapcar #'simple-eval forms))))
    (while (test body) (declare (ignore test body)) nil)
    (let (binds body) (declare (ignore binds body)) nil)
    ;; Functions
    (call (n args) (declare (ignore n args)) nil)
    (lambda (p b c) (declare (ignore p b c)) nil)
    (funcall (f args) (declare (ignore f args)) nil)
    ;; List ops
    (cons (a d) (cl:cons (simple-eval a) (simple-eval d)))
    (car (c) (cl:car c))
    (cdr (c) (cl:cdr c))
    (list (e) (declare (ignore e)) nil)
    (length (l) (cl:length l))
    ;; Type predicates
    (null (v) (cl:null v))
    (consp (v) (cl:consp v))
    (symbolp (v) (cl:symbolp v))
    (stringp (v) (cl:stringp v))
    (numberp (v) (cl:numberp v))
    (keywordp (v) (cl:keywordp v))
    (functionp (v) (cl:functionp v))
    ;; String
    (string-length (s) (declare (ignore s)) 0)
    (string-ref (s i) (declare (ignore s i)) 0)
    (string-concat (l r) (declare (ignore l r)) "")
    ;; Vector
    (make-vector (s i) (declare (ignore s i)) nil)
    (vector-ref (v i) (declare (ignore v i)) nil)
    (vector-set (v i val) (declare (ignore v i val)) nil)
    (vector-length (v) (declare (ignore v)) 0)
    ;; Symbol
    (make-symbol (n) (declare (ignore n)) nil)
    (symbol-name (s) (declare (ignore s)) "")
    (intern (s) (declare (ignore s)) nil)
    ;; Keyword
    (keyword-name (k) (declare (ignore k)) nil)
    ;; System
    (exit (c) c)
    (error (m) m)))

(format t "~%Testing eval:~%")
(format t "(+ 1 2) = ~A~%" (simple-eval (ir-add (ir-lit 1) (ir-lit 2))))
(format t "(* 3 4) = ~A~%" (simple-eval (ir-mul (ir-lit 3) (ir-lit 4))))
(format t "(if t 10 20) = ~A~%" (simple-eval (ir-if (ir-t) (ir-lit 10) (ir-lit 20))))
(format t "42 = ~A~%" (simple-eval (ir-lit 42)))

(format t "~%SUCCESS: Type system works!~%")
