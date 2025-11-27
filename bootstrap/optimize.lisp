;;; optimize.lisp - Nanopass optimization framework for Habu
;;; Each optimization is a small, focused IR-to-IR transformation

(in-package :habu)

;;; ============================================================
;;; Nanopass Infrastructure
;;; ============================================================

(defvar *optimization-passes* nil
  "List of (name . function) pairs for optimization passes")

(defvar *optimization-stats* (make-hash-table :test 'equal)
  "Statistics for each optimization pass")

(defun register-optimization (name function)
  "Register an optimization pass"
  (push (cons name function) *optimization-passes*))

(defun run-optimization (name ir)
  "Run a single optimization pass and track stats"
  (let* ((start (get-internal-real-time))
         (pass (assoc name *optimization-passes*))
         (result (if pass
                     (funcall (cdr pass) ir)
                     ir))
         (end (get-internal-real-time))
         (elapsed (/ (* 1000.0 (- end start)) internal-time-units-per-second)))
    (let ((stats (gethash name *optimization-stats*)))
      (if stats
          (setf (gethash name *optimization-stats*)
                (list (1+ (first stats))
                      (+ (second stats) elapsed)))
          (setf (gethash name *optimization-stats*)
                (list 1 elapsed))))
    result))

(defun run-all-optimizations (ir)
  "Run all registered optimization passes in order"
  (let ((result ir))
    (dolist (pass (reverse *optimization-passes*))
      (setq result (run-optimization (car pass) result)))
    result))

(defun print-optimization-stats ()
  "Print statistics for all optimization passes"
  (format t "~%Optimization Statistics:~%")
  (maphash (lambda (name stats)
             (format t "  ~A: ~D calls, ~,3F ms total~%"
                     name (first stats) (second stats)))
           *optimization-stats*))

(defun clear-optimization-stats ()
  "Clear all optimization statistics"
  (clrhash *optimization-stats*))

;;; ============================================================
;;; Pass 1: Constant Folding
;;; ============================================================

(defun fold-constants (ir)
  "Fold constant expressions at compile time.
   Transforms (add (lit a) (lit b)) -> (lit (+ a b))"
  (cond
    ((null ir) nil)
    ((not (consp ir)) ir)
    ;; Arithmetic with two literal operands
    ((and (has-tag ir 'add)
          (has-tag (cadr ir) 'lit)
          (has-tag (caddr ir) 'lit))
     (list 'lit (+ (cadr (cadr ir)) (cadr (caddr ir)))))
    ((and (has-tag ir 'sub)
          (has-tag (cadr ir) 'lit)
          (has-tag (caddr ir) 'lit))
     (list 'lit (- (cadr (cadr ir)) (cadr (caddr ir)))))
    ((and (has-tag ir 'mul)
          (has-tag (cadr ir) 'lit)
          (has-tag (caddr ir) 'lit))
     (list 'lit (* (cadr (cadr ir)) (cadr (caddr ir)))))
    ((and (has-tag ir 'div)
          (has-tag (cadr ir) 'lit)
          (has-tag (caddr ir) 'lit)
          (not (zerop (cadr (caddr ir)))))
     (list 'lit (truncate (cadr (cadr ir)) (cadr (caddr ir)))))
    ;; Identity operations
    ((and (has-tag ir 'add) (has-tag (cadr ir) 'lit) (zerop (cadr (cadr ir))))
     (fold-constants (caddr ir)))
    ((and (has-tag ir 'add) (has-tag (caddr ir) 'lit) (zerop (cadr (caddr ir))))
     (fold-constants (cadr ir)))
    ((and (has-tag ir 'sub) (has-tag (caddr ir) 'lit) (zerop (cadr (caddr ir))))
     (fold-constants (cadr ir)))
    ((and (has-tag ir 'mul) (has-tag (cadr ir) 'lit) (= 1 (cadr (cadr ir))))
     (fold-constants (caddr ir)))
    ((and (has-tag ir 'mul) (has-tag (caddr ir) 'lit) (= 1 (cadr (caddr ir))))
     (fold-constants (cadr ir)))
    ;; Multiplication by zero
    ((and (has-tag ir 'mul) (has-tag (cadr ir) 'lit) (zerop (cadr (cadr ir))))
     '(lit 0))
    ((and (has-tag ir 'mul) (has-tag (caddr ir) 'lit) (zerop (cadr (caddr ir))))
     '(lit 0))
    ;; Binary ops - recurse then try to fold
    ((or (has-tag ir 'add) (has-tag ir 'sub)
         (has-tag ir 'mul) (has-tag ir 'div))
     (let* ((left (fold-constants (cadr ir)))
            (right (fold-constants (caddr ir)))
            (new-ir (list (car ir) left right)))
       ;; Try folding again after recursion
       (if (and (has-tag left 'lit) (has-tag right 'lit))
           (fold-constants new-ir)
           new-ir)))
    ;; Comparison folding
    ((and (has-tag ir 'cmp-eq)
          (has-tag (cadr ir) 'lit)
          (has-tag (caddr ir) 'lit))
     (list 'lit (if (= (cadr (cadr ir)) (cadr (caddr ir))) 1 0)))
    ((and (has-tag ir 'cmp-lt)
          (has-tag (cadr ir) 'lit)
          (has-tag (caddr ir) 'lit))
     (list 'lit (if (< (cadr (cadr ir)) (cadr (caddr ir))) 1 0)))
    ((and (has-tag ir 'cmp-gt)
          (has-tag (cadr ir) 'lit)
          (has-tag (caddr ir) 'lit))
     (list 'lit (if (> (cadr (cadr ir)) (cadr (caddr ir))) 1 0)))
    ;; If with constant condition
    ((and (has-tag ir 'if-ir)
          (has-tag (cadr ir) 'lit))
     (if (not (zerop (cadr (cadr ir))))
         (fold-constants (caddr ir))
         (fold-constants (cadddr ir))))
    ;; Progn - fold each form
    ((has-tag ir 'progn-ir)
     (cons 'progn-ir (mapcar #'fold-constants (cdr ir))))
    ;; Let - fold bindings and body
    ((has-tag ir 'let-ir)
     (let ((bindings (mapcar #'fold-constants (cadr ir)))
           (body (fold-constants (caddr ir))))
       (list 'let-ir bindings body (cadddr ir) (nth 4 ir))))
    ;; Default - recurse into known node types
    ((or (has-tag ir 'car-ir) (has-tag ir 'cdr-ir)
         (has-tag ir 'null-ir) (has-tag ir 'consp-ir))
     (list (car ir) (fold-constants (cadr ir))))
    ((has-tag ir 'cons-ir)
     (list 'cons-ir (fold-constants (cadr ir)) (fold-constants (caddr ir))))
    ((has-tag ir 'if-ir)
     (list 'if-ir
           (fold-constants (cadr ir))
           (fold-constants (caddr ir))
           (fold-constants (cadddr ir))))
    ;; Call with folded args
    ((has-tag ir 'call-fn)
     (list 'call-fn (cadr ir) (mapcar #'fold-constants (caddr ir))))
    ;; Pass through everything else
    (t ir)))

(register-optimization 'constant-folding #'fold-constants)

;;; ============================================================
;;; Pass 2: Strength Reduction
;;; ============================================================

(defun reduce-strength (ir)
  "Replace expensive operations with cheaper equivalents.
   - (* x 2) -> (<< x 1)
   - (* x 4) -> (<< x 2)
   - (/ x 2) -> (>> x 1) for positive x"
  (cond
    ((null ir) nil)
    ((not (consp ir)) ir)
    ;; (* x 2^n) -> (<< x n)
    ((and (has-tag ir 'mul)
          (has-tag (caddr ir) 'lit)
          (power-of-two-p (cadr (caddr ir))))
     (let ((shift (log2-int (cadr (caddr ir)))))
       (list 'bsh (reduce-strength (cadr ir)) (list 'lit shift))))
    ((and (has-tag ir 'mul)
          (has-tag (cadr ir) 'lit)
          (power-of-two-p (cadr (cadr ir))))
     (let ((shift (log2-int (cadr (cadr ir)))))
       (list 'bsh (reduce-strength (caddr ir)) (list 'lit shift))))
    ;; Binary ops - recurse
    ((or (has-tag ir 'add) (has-tag ir 'sub)
         (has-tag ir 'mul) (has-tag ir 'div)
         (has-tag ir 'bsh) (has-tag ir 'band)
         (has-tag ir 'bor) (has-tag ir 'bxor))
     (list (car ir)
           (reduce-strength (cadr ir))
           (reduce-strength (caddr ir))))
    ;; Comparisons
    ((or (has-tag ir 'cmp-eq) (has-tag ir 'cmp-lt)
         (has-tag ir 'cmp-gt) (has-tag ir 'cmp-le)
         (has-tag ir 'cmp-ge))
     (list (car ir)
           (reduce-strength (cadr ir))
           (reduce-strength (caddr ir))))
    ;; Unary ops
    ((or (has-tag ir 'car-ir) (has-tag ir 'cdr-ir)
         (has-tag ir 'null-ir) (has-tag ir 'consp-ir))
     (list (car ir) (reduce-strength (cadr ir))))
    ((has-tag ir 'cons-ir)
     (list 'cons-ir (reduce-strength (cadr ir)) (reduce-strength (caddr ir))))
    ;; Control flow
    ((has-tag ir 'if-ir)
     (list 'if-ir
           (reduce-strength (cadr ir))
           (reduce-strength (caddr ir))
           (reduce-strength (cadddr ir))))
    ((has-tag ir 'progn-ir)
     (cons 'progn-ir (mapcar #'reduce-strength (cdr ir))))
    ((has-tag ir 'let-ir)
     (list 'let-ir
           (mapcar #'reduce-strength (cadr ir))
           (reduce-strength (caddr ir))
           (cadddr ir) (nth 4 ir)))
    ((has-tag ir 'call-fn)
     (list 'call-fn (cadr ir) (mapcar #'reduce-strength (caddr ir))))
    (t ir)))

(defun power-of-two-p (n)
  "Check if n is a power of 2"
  (and (integerp n)
       (> n 0)
       (zerop (logand n (1- n)))))

(defun log2-int (n)
  "Integer log base 2"
  (if (<= n 1)
      0
      (1+ (log2-int (ash n -1)))))

(register-optimization 'strength-reduction #'reduce-strength)

;;; ============================================================
;;; Pass 3: Dead Code Elimination
;;; ============================================================

(defun eliminate-dead-code (ir)
  "Remove unreachable code.
   - (progn x) -> x
   - (if (lit 1) then else) -> then
   - (if (lit 0) then else) -> else"
  (cond
    ((null ir) nil)
    ((not (consp ir)) ir)
    ;; Single-form progn
    ((and (has-tag ir 'progn-ir) (= 2 (length ir)))
     (eliminate-dead-code (cadr ir)))
    ;; Empty progn
    ((and (has-tag ir 'progn-ir) (= 1 (length ir)))
     '(lit 0))
    ;; Progn with constants in non-final position
    ((has-tag ir 'progn-ir)
     (let ((forms (remove-if (lambda (f)
                               (and (has-tag f 'lit)
                                    (not (eq f (car (last (cdr ir)))))))
                             (cdr ir))))
       (if (= 1 (length forms))
           (eliminate-dead-code (car forms))
           (cons 'progn-ir (mapcar #'eliminate-dead-code forms)))))
    ;; If with constant condition (already handled in fold-constants, but keep for completeness)
    ((and (has-tag ir 'if-ir) (has-tag (cadr ir) 'lit))
     (if (not (zerop (cadr (cadr ir))))
         (eliminate-dead-code (caddr ir))
         (eliminate-dead-code (cadddr ir))))
    ;; Recurse into structures
    ((has-tag ir 'if-ir)
     (list 'if-ir
           (eliminate-dead-code (cadr ir))
           (eliminate-dead-code (caddr ir))
           (eliminate-dead-code (cadddr ir))))
    ((has-tag ir 'progn-ir)
     (cons 'progn-ir (mapcar #'eliminate-dead-code (cdr ir))))
    ((has-tag ir 'let-ir)
     (list 'let-ir
           (mapcar #'eliminate-dead-code (cadr ir))
           (eliminate-dead-code (caddr ir))
           (cadddr ir) (nth 4 ir)))
    ((or (has-tag ir 'add) (has-tag ir 'sub)
         (has-tag ir 'mul) (has-tag ir 'div))
     (list (car ir)
           (eliminate-dead-code (cadr ir))
           (eliminate-dead-code (caddr ir))))
    ((has-tag ir 'call-fn)
     (list 'call-fn (cadr ir) (mapcar #'eliminate-dead-code (caddr ir))))
    (t ir)))

(register-optimization 'dead-code-elimination #'eliminate-dead-code)

;;; ============================================================
;;; Optimization Pipeline
;;; ============================================================

(defun optimize-ir (ir &key (passes '(constant-folding strength-reduction dead-code-elimination)))
  "Run specified optimization passes on IR"
  (let ((result ir))
    (dolist (pass passes)
      (setq result (run-optimization pass result)))
    result))

(defun optimize-function-ir (fn-ir)
  "Optimize a function's IR, preserving function metadata"
  (if (has-tag fn-ir 'fn-ir)
      (list 'fn-ir
            (cadr fn-ir)   ; name
            (caddr fn-ir)  ; params
            (optimize-ir (cadddr fn-ir))  ; body
            (nth 4 fn-ir)) ; metadata
      fn-ir))

;;; ============================================================
;;; Helper: has-tag (used by optimizer, avoids nc- prefix)
;;; ============================================================

(defun has-tag (ir tag)
  "Check if IR has the given tag"
  (and (consp ir) (eq (car ir) tag)))
