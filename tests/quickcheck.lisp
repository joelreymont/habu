;;;; test-properties.lisp - QuickCheck-style property tests for Habu
;;;;
;;;; Custom property testing framework designed to be portable to native Habu.
;;;; Features: generators with shrinking, property checking, counterexample minimization.
;;;;
;;;; No CLOS, no multiple values, no conditions - just functions and cons cells.

(in-package :habu)

;;; ============================================================
;;; Generator Framework
;;; ============================================================
;;;
;;; A generator is a cons: (gen-fn . shrink-fn)
;;;   gen-fn: () -> random-value
;;;   shrink-fn: value -> list of smaller values

(defun make-gen (gen-fn shrink-fn)
  "Create a generator with generation and shrinking functions."
  (cons gen-fn shrink-fn))

(defun gen-value (g)
  "Generate a random value from generator G."
  (funcall (car g)))

(defun shrink-value (g val)
  "Return list of smaller values for VAL using generator G's shrinker."
  (funcall (cdr g) val))

;;; ============================================================
;;; Core Generators with Shrinking
;;; ============================================================

(defun gen-int (min max)
  "Generator for integers in [MIN, MAX] with shrinking toward 0."
  (make-gen
   ;; Generate
   (lambda () (+ min (random (1+ (- max min)))))
   ;; Shrink toward 0 (or min if 0 not in range)
   (lambda (n)
     (let ((target (cond ((and (<= min 0) (>= max 0)) 0)
                         ((> min 0) min)
                         (t max))))
       (if (= n target)
           nil  ; Already minimal
           (let* ((half (truncate (+ n target) 2))
                  (step (if (> n target) -1 1))  ; Direction toward target
                  (adjacent (+ n step))          ; n-1 or n+1 toward target
                  (candidates nil))
             ;; Try the target first (biggest shrink)
             (when (/= n target)
               (push target candidates))
             ;; Try halfway point (binary search)
             (when (and (/= half n) (/= half target)
                        (>= half min) (<= half max))
               (push half candidates))
             ;; Try adjacent value (for finding exact boundaries)
             (when (and (/= adjacent n) (/= adjacent target)
                        (>= adjacent min) (<= adjacent max))
               (push adjacent candidates))
             (nreverse candidates)))))))

(defun gen-bool ()
  "Generator for booleans with shrinking toward nil."
  (make-gen
   (lambda () (= 1 (random 2)))
   (lambda (b) (if b (list nil) nil))))

(defun gen-one-of (choices)
  "Generator that picks from CHOICES, shrinks toward earlier choices."
  (let ((n (length choices)))
    (make-gen
     (lambda () (nth (random n) choices))
     (lambda (val)
       (let ((pos (position val choices)))
         (if (or (null pos) (= pos 0))
             nil
             (list (nth 0 choices))))))))

(defun gen-element (lst)
  "Generator that picks uniformly from LST, shrinks toward first element."
  (gen-one-of lst))

;;; ============================================================
;;; Composite Generators
;;; ============================================================

(defun gen-list-of (elem-gen min-len max-len)
  "Generator for lists of elements from ELEM-GEN, length in [MIN-LEN, MAX-LEN]."
  (make-gen
   ;; Generate
   (lambda ()
     (let ((len (+ min-len (random (1+ (- max-len min-len))))))
       (loop repeat len collect (gen-value elem-gen))))
   ;; Shrink: try removing elements, then shrink individual elements
   (lambda (lst)
     (let ((candidates nil)
           (len (length lst)))
       ;; Try removing each element (if above min-len)
       (when (> len min-len)
         (dotimes (i len)
           (push (append (subseq lst 0 i) (subseq lst (1+ i))) candidates)))
       ;; Try shrinking each element
       (dotimes (i len)
         (dolist (smaller (shrink-value elem-gen (nth i lst)))
           (push (append (subseq lst 0 i)
                         (list smaller)
                         (subseq lst (1+ i)))
                 candidates)))
       (nreverse candidates)))))

(defun gen-tuple (&rest gens)
  "Generator for fixed-size tuples, one element per generator in GENS."
  (make-gen
   ;; Generate
   (lambda ()
     (mapcar #'gen-value gens))
   ;; Shrink each position independently
   (lambda (tup)
     (let ((candidates nil))
       (loop for i from 0
             for g in gens
             for val in tup
             do (dolist (smaller (shrink-value g val))
                  (push (append (subseq tup 0 i)
                                (list smaller)
                                (subseq tup (1+ i)))
                        candidates)))
       (nreverse candidates)))))

;;; ============================================================
;;; ARM64-Specific Generators
;;; ============================================================

(defvar *register-keywords*
  '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7 :x8 :x9 :x10 :x11 :x12 :x13 :x14
    :x15 :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23 :x24 :x25 :x26 :x27
    :x28 :x29 :x30))

(defun gen-register ()
  "Generator for ARM64 registers :x0 to :x30, shrinks toward :x0."
  (make-gen
   (lambda () (nth (random 31) *register-keywords*))
   (lambda (reg)
     (let ((pos (position reg *register-keywords*)))
       (cond ((null pos) nil)
             ((= pos 0) nil)  ; Already :x0
             ((< pos 4) (list :x0))
             (t (list :x0 (nth (truncate pos 2) *register-keywords*))))))))

(defun gen-register-or-sp ()
  "Generator for ARM64 registers :x0 to :x30 or :sp, shrinks toward :x0."
  (make-gen
   (lambda ()
     (let ((n (random 32)))
       (if (= n 31) :sp (nth n *register-keywords*))))
   (lambda (reg)
     (if (eq reg :sp)
         (list :x0)
         (shrink-value (gen-register) reg)))))

(defun gen-imm12 ()
  "Generator for 12-bit immediates (0-4095), shrinks toward 0."
  (gen-int 0 4095))

(defun gen-imm16 ()
  "Generator for 16-bit immediates (0-65535), shrinks toward 0."
  (gen-int 0 65535))

(defun gen-branch-offset ()
  "Generator for branch offsets (instruction count), shrinks toward 0."
  (gen-int -8388608 8388607))

(defun gen-mem-offset ()
  "Generator for memory offsets (multiples of 8), shrinks toward 0."
  (make-gen
   (lambda () (* 8 (- (random 64) 32)))
   (lambda (off)
     (cond ((= off 0) nil)
           ((> off 0) (list 0 (* 8 (truncate (/ off 8) 2))))
           (t (list 0 (* 8 (truncate (/ off 8) 2))))))))

(defun gen-cond-code ()
  "Generator for condition codes, shrinks toward :eq."
  (gen-one-of '(:eq :ne :cs :cc :mi :pl :vs :vc :hi :ls :ge :lt :gt :le :al :nv)))

;;; ============================================================
;;; Property Checking with Shrinking
;;; ============================================================

(defvar *quickcheck-trials* 100
  "Number of random trials per property.")

(defvar *quickcheck-max-shrinks* 100
  "Maximum shrinking attempts per failure.")

(defun check-property-once (gen prop)
  "Test property once, return (value . result)."
  (let ((val (gen-value gen)))
    (cons val (funcall prop val))))

(defun shrink-failure (gen prop val shrinks-left)
  "Find minimal failing case by shrinking VAL."
  (if (<= shrinks-left 0)
      val  ; Shrink budget exhausted
      (let ((candidates (shrink-value gen val)))
        (labels ((try-shrinks (cs best)
                   (if (null cs)
                       best
                       (let ((c (car cs)))
                         (if (funcall prop c)
                             (try-shrinks (cdr cs) best)  ; This passes, try next
                             ;; Found smaller failure, recurse
                             (shrink-failure gen prop c (1- shrinks-left)))))))
          (try-shrinks candidates val)))))

(defun check-property (gen prop &optional (trials *quickcheck-trials*))
  "Check that PROP holds for TRIALS random values from GEN.
   Returns (:passed TRIALS) or (:failed ORIGINAL-VALUE SHRUNK-VALUE SHRINK-STEPS)."
  (labels ((run-trials (n)
             (if (<= n 0)
                 (list :passed trials)
                 (let* ((val (gen-value gen))
                        (result (funcall prop val)))
                   (if result
                       (run-trials (1- n))
                       ;; Found failure, shrink it
                       (let ((shrunk (shrink-failure gen prop val *quickcheck-max-shrinks*)))
                         (list :failed val shrunk)))))))
    (run-trials trials)))

;;; ============================================================
;;; Property Definition Macro
;;; ============================================================

(defvar *property-results* nil)
(defvar *property-pass-count* 0)
(defvar *property-fail-count* 0)

(defun reset-property-stats ()
  (setf *property-pass-count* 0
        *property-fail-count* 0
        *property-results* nil))

(defmacro defproperty (name (&rest bindings) &body body)
  "Define a property test.
   BINDINGS are (var generator) pairs.
   BODY should return T if property holds."
  (let* ((vars (mapcar #'car bindings))
         (gens (mapcar #'cadr bindings))
         (tuple-gen (if (= 1 (length gens))
                        (car gens)
                        `(gen-tuple ,@gens)))
         (prop-body (if (= 1 (length vars))
                        `(lambda (,(car vars)) ,@body)
                        `(lambda (args)
                           (let ,(loop for v in vars
                                       for i from 0
                                       collect `(,v (nth ,i args)))
                             ,@body)))))
    `(defun ,name (&optional (trials *quickcheck-trials*))
       (let* ((gen ,tuple-gen)
              (prop ,prop-body)
              (result (check-property gen prop trials)))
         (if (eq (car result) :passed)
             (progn
               (incf *property-pass-count*)
               (push (list ',name :passed trials) *property-results*)
               t)
             (progn
               (incf *property-fail-count*)
               (push (list ',name :failed
                           :original (second result)
                           :shrunk (third result))
                     *property-results*)
               nil))))))

;;; ============================================================
;;; ARM64 Instruction Decoders (for roundtrip testing)
;;; ============================================================

(defun bytes-to-word (bytes)
  "Convert 4 bytes (little-endian) to 32-bit word."
  (logior (nth 0 bytes)
          (ash (nth 1 bytes) 8)
          (ash (nth 2 bytes) 16)
          (ash (nth 3 bytes) 24)))

(defun decode-reg (bits)
  "Decode 5-bit register field to keyword."
  (if (= bits 31)
      :sp
      (nth bits *register-keywords*)))

(defun decode-add-sub-imm (bytes)
  "Decode ADD/SUB immediate. Returns (op rd rn imm) or nil."
  (when (= (length bytes) 4)
    (let* ((word (bytes-to-word bytes))
           (rd (logand word #x1F))
           (rn (logand (ash word -5) #x1F))
           (imm12 (logand (ash word -10) #xFFF))
           (shift (logand (ash word -22) #x1))
           (op-bit (logand (ash word -30) #x1))
           (sf (logand (ash word -31) #x1)))
      (when (and (= sf 1)
                 (= (logand (ash word -23) #x7F) #x22))
        (list (if (= op-bit 0) :add :sub)
              (decode-reg rd)
              (decode-reg rn)
              (if (= shift 1) (ash imm12 12) imm12))))))

(defun decode-movz (bytes)
  "Decode MOVZ. Returns (rd imm16 shift) or nil."
  (when (= (length bytes) 4)
    (let* ((word (bytes-to-word bytes))
           (rd (logand word #x1F))
           (imm16 (logand (ash word -5) #xFFFF))
           (hw (logand (ash word -21) #x3))
           (opc (logand (ash word -29) #x3))  ; 2 bits for opc
           (fixed (logand (ash word -23) #x3F))  ; should be 100101 = 37
           (sf (logand (ash word -31) #x1)))
      (when (and (= sf 1) (= opc 2) (= fixed 37))  ; opc=10 for MOVZ
        (list (decode-reg rd) imm16 (* hw 16))))))

(defun decode-logical-reg (bytes)
  "Decode ORR/AND/EOR register. Returns (op rd rn rm) or nil."
  (when (= (length bytes) 4)
    (let* ((word (bytes-to-word bytes))
           (rd (logand word #x1F))
           (rn (logand (ash word -5) #x1F))
           (imm6 (logand (ash word -10) #x3F))
           (rm (logand (ash word -16) #x1F))
           (n-bit (logand (ash word -21) #x1))
           (shift-type (logand (ash word -22) #x3))
           (opc (logand (ash word -29) #x3))
           (sf (logand (ash word -31) #x1)))
      (when (and (= sf 1) (= imm6 0) (= shift-type 0) (= n-bit 0))
        (list (case opc (0 :and) (1 :orr) (2 :eor) (3 :ands))
              (decode-reg rd)
              (decode-reg rn)
              (decode-reg rm))))))

;;; ============================================================
;;; ARM64 Encoding Properties
;;; ============================================================

;; Size properties: all instructions are exactly 4 bytes

(defproperty prop-add-imm-size ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (= 4 (length (arm64:add rd rn imm :imm t))))

(defproperty prop-sub-imm-size ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (= 4 (length (arm64:sub rd rn imm :imm t))))

(defproperty prop-movz-size ((rd (gen-register)) (imm (gen-imm16)))
  (= 4 (length (arm64:movz rd imm))))

(defproperty prop-movk-size ((rd (gen-register)) (imm (gen-imm16)))
  (= 4 (length (arm64:movk rd imm))))

(defproperty prop-orr-reg-size ((rd (gen-register)) (rn (gen-register)) (rm (gen-register)))
  (= 4 (length (arm64:orr rd rn rm))))

(defproperty prop-and-reg-size ((rd (gen-register)) (rn (gen-register)) (rm (gen-register)))
  (= 4 (length (arm64:and* rd rn rm))))

(defproperty prop-eor-reg-size ((rd (gen-register)) (rn (gen-register)) (rm (gen-register)))
  (= 4 (length (arm64:eor rd rn rm))))

(defproperty prop-ldr-size ((rt (gen-register)) (rn (gen-register-or-sp)))
  (= 4 (length (arm64:ldr rt rn :offset 0))))

(defproperty prop-str-size ((rt (gen-register)) (rn (gen-register-or-sp)))
  (= 4 (length (arm64:str rt rn :offset 0))))

(defproperty prop-bl-size ((offset (gen-branch-offset)))
  (= 4 (length (arm64:bl offset))))

(defproperty prop-b-size ((offset (gen-branch-offset)))
  (= 4 (length (arm64:b offset))))

(defproperty prop-ret-size ((dummy (gen-int 0 0)))
  (declare (ignore dummy))
  (= 4 (length (arm64:ret))))

;; Roundtrip properties: encode then decode should give original values

(defproperty prop-add-imm-roundtrip ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (let* ((encoded (arm64:add rd rn imm :imm t))
         (decoded (decode-add-sub-imm encoded)))
    (and decoded
         (eq (first decoded) :add)
         (eq (second decoded) rd)
         (eq (third decoded) rn)
         (= (fourth decoded) imm))))

(defproperty prop-sub-imm-roundtrip ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (let* ((encoded (arm64:sub rd rn imm :imm t))
         (decoded (decode-add-sub-imm encoded)))
    (and decoded
         (eq (first decoded) :sub)
         (eq (second decoded) rd)
         (eq (third decoded) rn)
         (= (fourth decoded) imm))))

(defproperty prop-movz-roundtrip ((rd (gen-register)) (imm (gen-imm16)))
  (let* ((encoded (arm64:movz rd imm))
         (decoded (decode-movz encoded)))
    (and decoded
         (eq (first decoded) rd)
         (= (second decoded) imm)
         (= (third decoded) 0))))

(defproperty prop-orr-reg-roundtrip ((rd (gen-register)) (rn (gen-register)) (rm (gen-register)))
  (let* ((encoded (arm64:orr rd rn rm))
         (decoded (decode-logical-reg encoded)))
    (and decoded
         (eq (first decoded) :orr)
         (eq (second decoded) rd)
         (eq (third decoded) rn)
         (eq (fourth decoded) rm))))

;;; ============================================================
;;; Tagged Value Properties
;;; ============================================================

(defproperty prop-fixnum-tag-roundtrip ((n (gen-int -1000000 1000000)))
  (let ((tagged (ash n 4)))
    (= (ash tagged -4) n)))

(defproperty prop-fixnum-tag-is-zero ((n (gen-int -1000000 1000000)))
  (let ((tagged (ash n 4)))
    (= (logand tagged #xF) 0)))

(defproperty prop-large-fixnum-roundtrip ((n (gen-int -2305843009213693952 2305843009213693951)))
  (let ((tagged (ash n 4)))
    (= (ash tagged -4) n)))

;;; ============================================================
;;; Code Size Properties
;;; ============================================================
;;; Note: code-size in SBCL mode expects flattened byte lists.
;;; These tests verify that instruction sizes are correct.

(defproperty prop-instruction-size-nop ((dummy (gen-int 0 0)))
  (declare (ignore dummy))
  (= 4 (length (arm64:nop))))

(defproperty prop-instruction-size-add ((rd (gen-register)) (rn (gen-register)) (imm (gen-imm12)))
  (= 4 (length (arm64:add rd rn imm :imm t))))

(defproperty prop-flatten-preserves-bytes ((n (gen-int 1 10)))
  (let* ((instrs (loop repeat n collect (arm64:nop)))
         (flat (flatten-code-keep-markers-and-calls instrs)))
    ;; Each NOP is 4 bytes, so n NOPs should be 4*n bytes
    (= (length flat) (* n 4))))

(defproperty prop-flatten-nested ((n (gen-int 1 5)))
  (let* ((inner (loop repeat n collect (arm64:ret)))
         (outer (list inner inner))
         (flat (flatten-code-keep-markers-and-calls outer)))
    ;; 2 groups of n RETs, each 4 bytes
    (= (length flat) (* 2 n 4))))

;;; ============================================================
;;; Unit Tests for QuickCheck Framework
;;; ============================================================
;;; These tests verify that the QuickCheck infrastructure works correctly.

(defun test-gen-int-range ()
  "Test that gen-int produces values within range."
  (let ((g (gen-int 10 20))
        (all-in-range t))
    (dotimes (i 100)
      (let ((v (gen-value g)))
        (unless (and (>= v 10) (<= v 20))
          (setf all-in-range nil))))
    (if all-in-range
        (progn (format t "  [PASS] gen-int produces values in range~%") t)
        (progn (format t "  [FAIL] gen-int produced out-of-range value~%") nil))))

(defun test-gen-int-negative-range ()
  "Test that gen-int works with negative ranges."
  (let ((g (gen-int -50 -10))
        (all-in-range t))
    (dotimes (i 100)
      (let ((v (gen-value g)))
        (unless (and (>= v -50) (<= v -10))
          (setf all-in-range nil))))
    (if all-in-range
        (progn (format t "  [PASS] gen-int works with negative range~%") t)
        (progn (format t "  [FAIL] gen-int failed with negative range~%") nil))))

(defun test-gen-bool ()
  "Test that gen-bool produces only t or nil."
  (let ((g (gen-bool))
        (saw-t nil)
        (saw-nil nil)
        (all-valid t))
    (dotimes (i 100)
      (let ((v (gen-value g)))
        (cond ((eq v t) (setf saw-t t))
              ((eq v nil) (setf saw-nil t))
              (t (setf all-valid nil)))))
    (if (and all-valid saw-t saw-nil)
        (progn (format t "  [PASS] gen-bool produces t and nil~%") t)
        (progn (format t "  [FAIL] gen-bool didn't produce both t and nil~%") nil))))

(defun test-gen-one-of ()
  "Test that gen-one-of picks from choices."
  (let ((g (gen-one-of '(:a :b :c)))
        (seen (make-hash-table))
        (all-valid t))
    (dotimes (i 100)
      (let ((v (gen-value g)))
        (unless (member v '(:a :b :c))
          (setf all-valid nil))
        (setf (gethash v seen) t)))
    (if (and all-valid
             (gethash :a seen)
             (gethash :b seen)
             (gethash :c seen))
        (progn (format t "  [PASS] gen-one-of picks from choices~%") t)
        (progn (format t "  [FAIL] gen-one-of didn't cover all choices~%") nil))))

(defun test-gen-register ()
  "Test that gen-register produces valid register keywords."
  (let ((g (gen-register))
        (all-valid t))
    (dotimes (i 100)
      (let ((v (gen-value g)))
        (unless (member v *register-keywords*)
          (setf all-valid nil))))
    (if all-valid
        (progn (format t "  [PASS] gen-register produces valid registers~%") t)
        (progn (format t "  [FAIL] gen-register produced invalid register~%") nil))))

(defun test-gen-list-of ()
  "Test that gen-list-of produces lists of correct length."
  (let ((g (gen-list-of (gen-int 0 10) 3 5))
        (all-valid t))
    (dotimes (i 50)
      (let* ((v (gen-value g))
             (len (length v)))
        (unless (and (>= len 3) (<= len 5))
          (setf all-valid nil))
        (dolist (elem v)
          (unless (and (integerp elem) (>= elem 0) (<= elem 10))
            (setf all-valid nil)))))
    (if all-valid
        (progn (format t "  [PASS] gen-list-of produces valid lists~%") t)
        (progn (format t "  [FAIL] gen-list-of produced invalid list~%") nil))))

(defun test-gen-tuple ()
  "Test that gen-tuple produces correct tuples."
  (let ((g (gen-tuple (gen-int 0 10) (gen-bool) (gen-one-of '(:x :y))))
        (all-valid t))
    (dotimes (i 50)
      (let ((v (gen-value g)))
        (unless (and (= (length v) 3)
                     (integerp (first v))
                     (>= (first v) 0)
                     (<= (first v) 10)
                     (or (eq (second v) t) (eq (second v) nil))
                     (member (third v) '(:x :y)))
          (setf all-valid nil))))
    (if all-valid
        (progn (format t "  [PASS] gen-tuple produces valid tuples~%") t)
        (progn (format t "  [FAIL] gen-tuple produced invalid tuple~%") nil))))

(defun test-shrink-int-toward-zero ()
  "Test that shrink-int shrinks toward 0."
  (let ((g (gen-int -100 100)))
    ;; Shrink 50 should include 0 and 25
    (let ((shrinks (shrink-value g 50)))
      (if (and (member 0 shrinks)
               (member 25 shrinks))
          (progn (format t "  [PASS] shrink-int shrinks toward 0~%") t)
          (progn (format t "  [FAIL] shrink-int didn't shrink toward 0: ~S~%" shrinks) nil)))))

(defun test-shrink-int-at-target ()
  "Test that shrink-int returns nil at target."
  (let ((g (gen-int -100 100)))
    (let ((shrinks (shrink-value g 0)))
      (if (null shrinks)
          (progn (format t "  [PASS] shrink-int returns nil at target~%") t)
          (progn (format t "  [FAIL] shrink-int returned ~S at target~%" shrinks) nil)))))

(defun test-shrink-bool ()
  "Test that shrink-bool shrinks t to nil."
  (let ((g (gen-bool)))
    (let ((shrinks-t (shrink-value g t))
          (shrinks-nil (shrink-value g nil)))
      (if (and (equal shrinks-t '(nil))
               (null shrinks-nil))
          (progn (format t "  [PASS] shrink-bool works correctly~%") t)
          (progn (format t "  [FAIL] shrink-bool: t->~S nil->~S~%" shrinks-t shrinks-nil) nil)))))

(defun test-shrink-one-of ()
  "Test that shrink-one-of shrinks toward first element."
  (let ((g (gen-one-of '(:first :second :third))))
    (let ((shrinks (shrink-value g :third)))
      (if (equal shrinks '(:first))
          (progn (format t "  [PASS] shrink-one-of shrinks to first~%") t)
          (progn (format t "  [FAIL] shrink-one-of: ~S~%" shrinks) nil)))))

(defun test-shrink-failure-finds-minimal ()
  "Test that shrink-failure finds minimal failing case."
  (let ((g (gen-int 0 100))
        ;; Property: n < 10 (so 10+ fails)
        (prop (lambda (n) (< n 10))))
    ;; Start with a larger failing value
    (let ((minimal (shrink-failure g prop 50 100)))
      (if (= minimal 10)
          (progn (format t "  [PASS] shrink-failure finds minimal case~%") t)
          (progn (format t "  [FAIL] shrink-failure got ~S, expected 10~%" minimal) nil)))))

(defun test-check-property-passes ()
  "Test that check-property reports success for always-true property."
  (let ((g (gen-int 0 100))
        (always-true (lambda (n) (declare (ignore n)) t)))
    (let ((result (check-property g always-true 50)))
      (if (eq (car result) :passed)
          (progn (format t "  [PASS] check-property reports success~%") t)
          (progn (format t "  [FAIL] check-property: ~S~%" result) nil)))))

(defun test-check-property-fails ()
  "Test that check-property reports failure with shrunk value."
  (let ((g (gen-int 0 100))
        ;; Property fails for anything >= 5
        (prop (lambda (n) (< n 5))))
    (let ((result (check-property g prop 100)))
      (if (and (eq (car result) :failed)
               ;; Shrunk value should be 5 (minimal failing)
               (= (third result) 5))
          (progn (format t "  [PASS] check-property reports failure with shrunk value~%") t)
          (progn (format t "  [FAIL] check-property: ~S~%" result) nil)))))

(defun test-bytes-to-word ()
  "Test bytes-to-word conversion."
  (if (and (= (bytes-to-word '(#x01 #x02 #x03 #x04)) #x04030201)
           (= (bytes-to-word '(#xFF #x00 #x00 #x00)) #x000000FF)
           (= (bytes-to-word '(#x00 #xFF #x00 #x00)) #x0000FF00))
      (progn (format t "  [PASS] bytes-to-word works~%") t)
      (progn (format t "  [FAIL] bytes-to-word incorrect~%") nil)))

(defun test-decode-reg ()
  "Test register decoding."
  (if (and (eq (decode-reg 0) :x0)
           (eq (decode-reg 15) :x15)
           (eq (decode-reg 30) :x30)
           (eq (decode-reg 31) :sp))
      (progn (format t "  [PASS] decode-reg works~%") t)
      (progn (format t "  [FAIL] decode-reg incorrect~%") nil)))

(defun run-quickcheck-unit-tests ()
  "Run all QuickCheck framework unit tests."
  (format t "~%=== QuickCheck Framework Unit Tests ===~%~%")
  (let ((pass 0)
        (fail 0))
    (format t "Generator tests:~%")
    (if (test-gen-int-range) (incf pass) (incf fail))
    (if (test-gen-int-negative-range) (incf pass) (incf fail))
    (if (test-gen-bool) (incf pass) (incf fail))
    (if (test-gen-one-of) (incf pass) (incf fail))
    (if (test-gen-register) (incf pass) (incf fail))
    (if (test-gen-list-of) (incf pass) (incf fail))
    (if (test-gen-tuple) (incf pass) (incf fail))

    (format t "~%Shrinking tests:~%")
    (if (test-shrink-int-toward-zero) (incf pass) (incf fail))
    (if (test-shrink-int-at-target) (incf pass) (incf fail))
    (if (test-shrink-bool) (incf pass) (incf fail))
    (if (test-shrink-one-of) (incf pass) (incf fail))
    (if (test-shrink-failure-finds-minimal) (incf pass) (incf fail))

    (format t "~%Property runner tests:~%")
    (if (test-check-property-passes) (incf pass) (incf fail))
    (if (test-check-property-fails) (incf pass) (incf fail))

    (format t "~%Decoder tests:~%")
    (if (test-bytes-to-word) (incf pass) (incf fail))
    (if (test-decode-reg) (incf pass) (incf fail))

    (format t "~%QuickCheck Unit Tests: ~D passed, ~D failed~%" pass fail)
    (values (= fail 0) pass fail)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(defun run-property (name trials)
  "Run a single property test and report result."
  (let ((result (funcall name trials)))
    (format t "  [~A] ~A~%" (if result "PASS" "FAIL") name)
    (unless result
      (let ((info (car *property-results*)))
        (when (eq (second info) :failed)
          (format t "         Original: ~S~%" (getf (cddr info) :original))
          (format t "         Shrunk:   ~S~%" (getf (cddr info) :shrunk)))))
    result))

(defun run-property-tests (&optional (trials *quickcheck-trials*))
  "Run all property tests."
  ;; First run unit tests for the framework itself
  (multiple-value-bind (unit-ok unit-pass unit-fail)
      (run-quickcheck-unit-tests)
    (declare (ignore unit-ok))

    (format t "~%=== Property-Based Tests (~D trials each) ===~%~%" trials)
    (reset-property-stats)

    ;; ARM64 size properties
    (format t "ARM64 instruction size invariants:~%")
    (run-property 'prop-add-imm-size trials)
    (run-property 'prop-sub-imm-size trials)
    (run-property 'prop-movz-size trials)
    (run-property 'prop-movk-size trials)
    (run-property 'prop-orr-reg-size trials)
    (run-property 'prop-and-reg-size trials)
    (run-property 'prop-eor-reg-size trials)
    (run-property 'prop-ldr-size trials)
    (run-property 'prop-str-size trials)
    (run-property 'prop-bl-size trials)
    (run-property 'prop-b-size trials)
    (run-property 'prop-ret-size trials)

    ;; ARM64 roundtrip properties
    (format t "~%ARM64 encoding roundtrips:~%")
    (run-property 'prop-add-imm-roundtrip trials)
    (run-property 'prop-sub-imm-roundtrip trials)
    (run-property 'prop-movz-roundtrip trials)
    (run-property 'prop-orr-reg-roundtrip trials)

    ;; Tagged value properties
    (format t "~%Tagged value invariants:~%")
    (run-property 'prop-fixnum-tag-roundtrip trials)
    (run-property 'prop-fixnum-tag-is-zero trials)
    (run-property 'prop-large-fixnum-roundtrip trials)

    ;; Code generation properties
    (format t "~%Code generation:~%")
    (run-property 'prop-instruction-size-nop trials)
    (run-property 'prop-instruction-size-add trials)
    (run-property 'prop-flatten-preserves-bytes trials)
    (run-property 'prop-flatten-nested trials)

    ;; Summary
    (format t "~%Property Tests: ~D passed, ~D failed~%"
            *property-pass-count* *property-fail-count*)
    (format t "Total: ~D unit + ~D property = ~D passed, ~D failed~%"
            unit-pass *property-pass-count*
            (+ unit-pass *property-pass-count*)
            (+ unit-fail *property-fail-count*))

    (values (and (= unit-fail 0) (= *property-fail-count* 0))
            (+ unit-pass *property-pass-count*)
            (+ unit-fail *property-fail-count*))))
