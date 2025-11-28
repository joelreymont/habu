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
    ;; Progn - fold each form (structure: (progn-ir (form1 form2 ...)))
    ((has-tag ir 'progn-ir)
     (list 'progn-ir (mapcar #'fold-constants (cadr ir))))
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
    ;; Progn (structure: (progn-ir (form1 form2 ...)))
    ((has-tag ir 'progn-ir)
     (list 'progn-ir (mapcar #'reduce-strength (cadr ir))))
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
   - (progn-ir (x)) -> x (single form)
   - (progn-ir ()) -> (lit 0) (empty progn)
   - (if (lit 1) then else) -> then
   - (if (lit 0) then else) -> else
   Note: progn-ir structure is (progn-ir (form1 form2 ...)) where cadr is a list of forms"
  (cond
    ((null ir) nil)
    ((not (consp ir)) ir)
    ;; Handle progn-ir: structure is (progn-ir forms-list)
    ((has-tag ir 'progn-ir)
     (let* ((forms-list (cadr ir))
            ;; Remove constant literals from non-final positions
            (filtered (if (null forms-list)
                          nil
                          (let ((last-form (car (last forms-list))))
                            (remove-if (lambda (f)
                                         (and (has-tag f 'lit)
                                              (not (eq f last-form))))
                                       forms-list)))))
       (cond
         ;; Empty progn
         ((null filtered) '(lit 0))
         ;; Single-form progn
         ((= 1 (length filtered))
          (eliminate-dead-code (car filtered)))
         ;; Multiple forms - keep as progn-ir
         (t (list 'progn-ir (mapcar #'eliminate-dead-code filtered))))))
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
    ;; Note: progn-ir is handled above
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
;;; Pass 4: Self-Tail-Call to Loop Conversion
;;; ============================================================

;;; Convert self-tail-calls to loops for zero-overhead recursion.
;;; Transform: (defun f (args) ... (f new-args) ...)
;;; Into:      (defun f (args) (loop-ir body-with-continue))
;;; Where self-calls become: (continue-ir new-args)
;;;
;;; The loop-ir/continue-ir nodes are handled specially in codegen:
;;; - loop-ir generates a loop label at start, then evaluates body
;;; - continue-ir evaluates new args, stores to params, jumps to loop start

(defun convert-self-tail-calls (ir fn-name param-count)
  "Convert self-tail-calls in tail position to continue-ir nodes.
   fn-name is the name of the function we're in.
   param-count is the number of parameters (for generating correct setqs)."
  (cond
    ((null ir) nil)
    ((not (consp ir)) ir)
    ;; Self-call in tail position -> convert to continue
    ((and (has-tag ir 'call-fn)
          (eq (cadr ir) fn-name))
     (list 'continue-ir (mapcar #'convert-non-tail (caddr ir))))
    ;; if-ir: both branches are in tail position
    ((has-tag ir 'if-ir)
     (list 'if-ir
           (convert-non-tail (cadr ir))
           (convert-self-tail-calls (caddr ir) fn-name param-count)
           (convert-self-tail-calls (cadddr ir) fn-name param-count)))
    ;; progn-ir: last form is in tail position
    ((has-tag ir 'progn-ir)
     (let ((forms (cadr ir)))
       (if (null forms)
           ir
           (list 'progn-ir
                 (append (mapcar #'convert-non-tail (butlast forms))
                         (list (convert-self-tail-calls (car (last forms)) fn-name param-count)))))))
    ;; let-ir: body is in tail position
    ((has-tag ir 'let-ir)
     (list 'let-ir
           (mapcar #'convert-non-tail (cadr ir))
           (convert-self-tail-calls (caddr ir) fn-name param-count)
           (cadddr ir) (nth 4 ir)))
    ;; Everything else: not in tail position, don't convert
    (t (convert-non-tail ir))))

(defun convert-non-tail (ir)
  "Process IR that is NOT in tail position - don't convert any calls."
  (cond
    ((null ir) nil)
    ((not (consp ir)) ir)
    ;; call-fn stays as call-fn
    ((has-tag ir 'call-fn)
     (list 'call-fn (cadr ir) (mapcar #'convert-non-tail (caddr ir))))
    ;; Recurse into structures
    ((has-tag ir 'if-ir)
     (list 'if-ir
           (convert-non-tail (cadr ir))
           (convert-non-tail (caddr ir))
           (convert-non-tail (cadddr ir))))
    ((has-tag ir 'progn-ir)
     (list 'progn-ir (mapcar #'convert-non-tail (cadr ir))))
    ((has-tag ir 'let-ir)
     (list 'let-ir
           (mapcar #'convert-non-tail (cadr ir))
           (convert-non-tail (caddr ir))
           (cadddr ir) (nth 4 ir)))
    ((or (has-tag ir 'add) (has-tag ir 'sub)
         (has-tag ir 'mul) (has-tag ir 'div)
         (has-tag ir 'cmp-eq) (has-tag ir 'cmp-lt)
         (has-tag ir 'cmp-gt) (has-tag ir 'cmp-le)
         (has-tag ir 'cmp-ge))
     (list (car ir) (convert-non-tail (cadr ir)) (convert-non-tail (caddr ir))))
    ((or (has-tag ir 'car-ir) (has-tag ir 'cdr-ir)
         (has-tag ir 'null-ir) (has-tag ir 'consp-ir))
     (list (car ir) (convert-non-tail (cadr ir))))
    ((has-tag ir 'cons-ir)
     (list 'cons-ir (convert-non-tail (cadr ir)) (convert-non-tail (caddr ir))))
    (t ir)))

(defun has-self-tail-call-p (ir fn-name)
  "Check if IR contains a self-tail-call to fn-name in tail position."
  (cond
    ((null ir) nil)
    ((not (consp ir)) nil)
    ((and (has-tag ir 'call-fn) (eq (cadr ir) fn-name)) t)
    ((has-tag ir 'if-ir)
     (or (has-self-tail-call-p (caddr ir) fn-name)
         (has-self-tail-call-p (cadddr ir) fn-name)))
    ((has-tag ir 'progn-ir)
     (let ((forms (cadr ir)))
       (and forms (has-self-tail-call-p (car (last forms)) fn-name))))
    ((has-tag ir 'let-ir)
     (has-self-tail-call-p (caddr ir) fn-name))
    (t nil)))

(defun wrap-with-loop (ir)
  "Wrap IR in a loop-ir if it contains continue-ir nodes."
  (if (contains-continue-p ir)
      (list 'loop-ir ir)
      ir))

(defun contains-continue-p (ir)
  "Check if IR contains any continue-ir nodes."
  (cond
    ((null ir) nil)
    ((not (consp ir)) nil)
    ((has-tag ir 'continue-ir) t)
    ((has-tag ir 'if-ir)
     (or (contains-continue-p (caddr ir))
         (contains-continue-p (cadddr ir))))
    ((has-tag ir 'progn-ir)
     (some #'contains-continue-p (cadr ir)))
    ((has-tag ir 'let-ir)
     (or (some #'contains-continue-p (cadr ir))
         (contains-continue-p (caddr ir))))
    (t nil)))

;;; ============================================================
;;; Pass 5: Let-Flattening
;;; ============================================================

(defun flatten-let (ir)
  "Flatten consecutive nested let-ir nodes into a single let-ir.
   This reduces IR nesting depth from 100+ levels to just a few.
   Example: (let-ir ((x 1)) (let-ir ((y 2)) body)) -> (let-ir ((x 1) (y 2)) body)"
  (cond
    ((null ir) nil)
    ((not (consp ir)) ir)
    ;; Flatten nested let-ir
    ((has-tag ir 'let-ir)
     (let* ((vals (cadr ir))
            (body-ir (caddr ir))
            (count (cadddr ir))
            (offsets (nth 4 ir)))
       ;; If body is also a let-ir, merge them
       (if (and (consp body-ir) (has-tag body-ir 'let-ir))
           (let* ((inner-vals (cadr body-ir))
                  (inner-body (caddr body-ir))
                  (inner-count (cadddr body-ir))
                  (inner-offsets (nth 4 body-ir))
                  ;; Merge and recursively flatten
                  (merged
                   (list 'let-ir
                         (append (mapcar #'flatten-let vals)
                                 (mapcar #'flatten-let inner-vals))
                         (flatten-let inner-body)
                         (+ count inner-count)
                         (append offsets
                                 (mapcar (lambda (off) (+ off count))
                                         inner-offsets)))))
             ;; Recursively flatten the merged result
             (flatten-let merged))
           ;; Body is not a let-ir, just flatten the values and body
           (list 'let-ir
                 (mapcar #'flatten-let vals)
                 (flatten-let body-ir)
                 count
                 offsets))))
    ;; Recurse into other structures
    ((has-tag ir 'if-ir)
     (list 'if-ir
           (flatten-let (cadr ir))
           (flatten-let (caddr ir))
           (flatten-let (cadddr ir))))
    ((has-tag ir 'progn-ir)
     (list 'progn-ir (mapcar #'flatten-let (cadr ir))))
    ((or (has-tag ir 'add) (has-tag ir 'sub)
         (has-tag ir 'mul) (has-tag ir 'div))
     (list (car ir)
           (flatten-let (cadr ir))
           (flatten-let (caddr ir))))
    ((has-tag ir 'call-fn)
     (list 'call-fn (cadr ir) (mapcar #'flatten-let (caddr ir))))
    (t ir)))

(register-optimization 'let-flattening #'flatten-let)

;;; ============================================================
;;; Pass 6: Progn-Flattening
;;; ============================================================

(defun flatten-progn (ir)
  "Flatten nested progn-ir nodes into a single progn-ir.
   (progn (progn a b) c) => (progn a b c)"
  (cond
    ((null ir) nil)
    ((not (consp ir)) ir)
    ;; Flatten nested progn-ir
    ((has-tag ir 'progn-ir)
     (let* ((forms (cadr ir))
            (flattened-forms
             (apply #'append
                    (mapcar (lambda (form)
                              (let ((flat-form (flatten-progn form)))
                                (if (and (consp flat-form)
                                         (has-tag flat-form 'progn-ir))
                                    (cadr flat-form)
                                    (list flat-form))))
                            forms))))
       (if (= (length flattened-forms) 1)
           (car flattened-forms)
           (list 'progn-ir flattened-forms))))
    ;; Recurse into other structures
    ((has-tag ir 'if-ir)
     (list 'if-ir
           (flatten-progn (cadr ir))
           (flatten-progn (caddr ir))
           (flatten-progn (cadddr ir))))
    ((has-tag ir 'let-ir)
     (list 'let-ir
           (mapcar #'flatten-progn (cadr ir))
           (flatten-progn (caddr ir))
           (cadddr ir) (nth 4 ir)))
    ((or (has-tag ir 'add) (has-tag ir 'sub)
         (has-tag ir 'mul) (has-tag ir 'div))
     (list (car ir)
           (flatten-progn (cadr ir))
           (flatten-progn (caddr ir))))
    ((has-tag ir 'call-fn)
     (list 'call-fn (cadr ir) (mapcar #'flatten-progn (caddr ir))))
    (t ir)))

(register-optimization 'progn-flattening #'flatten-progn)

;;; ============================================================
;;; Optimization Pipeline
;;; ============================================================

(defun optimize-ir (ir &key (passes '(let-flattening progn-flattening constant-folding strength-reduction dead-code-elimination)))
  "Run specified optimization passes on IR.
   Default passes now include let/progn flattening to reduce IR depth."
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
