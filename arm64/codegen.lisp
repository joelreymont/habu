;;;; ARM64 Code Generator
;;;; Pure Habu - no SBCL dependencies
;;;; Depends on: arm64/asm.lisp, common/utils.lisp

;;; Stack frame layout constants
(setq *stack-frame-size* #xFF0)
(setq *env-base-offset* #x180)
(setq *temp-slot-base* #x40)
(setq *temp-slot-guard* #x180)
(setq *arg-spill-base* #x200)
(setq *arg-spill-stride* #x8)

;;; Global state
(setq *runtime-addrs* nil)
(setq *collected-lambdas* nil)

;;; Calculate temp slot offset for given depth
(defun temp-slot-offset (depth)
  (let ((offset (+ *temp-slot-base* (* depth 8))))
    (if (>= offset *temp-slot-guard*)
        (+ *temp-slot-guard* (* (- depth (ash (- *temp-slot-guard* *temp-slot-base*) -3)) 8))
        offset)))

;;; Calculate arg spill offset for index
(defun arg-spill-offset (idx)
  (+ *arg-spill-base* (* idx *arg-spill-stride*)))

;;; Convert string to list of char codes
(defun string->char-codes (s)
  (labels ((iter (i acc)
             (if (>= i (length s))
                 (reverse acc)
                 (iter (+ i 1) (cons (char-code (char s i)) acc)))))
    (iter 0 nil)))

;;; Generate code for string literal
(defun codegen-string-from-chars (chars temp-depth)
  (let* ((len (length chars))
         (vec-slot (temp-slot-offset temp-depth))
         (alloc (append (movz 0 len)
                        (ldr-offset 11 19 56)
                        (blr 11)
                        (str-offset 0 31 vec-slot))))
    (labels ((gen-stores (cs idx acc)
               (if (nil? cs)
                   acc
                   (let* ((ch (car cs))
                          (tagged (ash ch 4))
                          (store (append
                                  (ldr-offset 0 31 vec-slot)
                                  (movz 1 idx)
                                  (if (< tagged #x10000)
                                      (movz 2 tagged)
                                      (load-addr 2 tagged))
                                  (ldr-offset 11 19 64)
                                  (blr 11))))
                     (gen-stores (cdr cs) (+ idx 1) (append acc store))))))
      (append (gen-stores chars 0 alloc)
              (ldr-offset 0 31 vec-slot)
              (ldr-offset 9 19 80)
              (blr 9)))))

;;; Generate code for vector literal
(defun codegen-vector-literal (elements runtime-addrs fn-offsets current-offset temp-depth)
  (let* ((len (length elements))
         (vec-slot (temp-slot-offset temp-depth))
         (alloc (append (movz 0 len)
                        (ldr-offset 11 19 56)
                        (blr 11)
                        (str-offset 0 31 vec-slot)))
         (cursor (if current-offset (+ current-offset (count-instrs alloc)) nil)))
    (labels ((gen-elems (els idx cur acc)
               (if (nil? els)
                   acc
                   (let* ((el-code (codegen-expr (car els) runtime-addrs fn-offsets cur (+ temp-depth 1)))
                          (store (append
                                  (mov-reg 2 0)
                                  (ldr-offset 0 31 vec-slot)
                                  (movz 1 idx)
                                  (ldr-offset 11 19 64)
                                  (blr 11)))
                          (step (+ (count-instrs el-code) (count-instrs store)))
                          (new-cur (if cur (+ cur step) nil)))
                     (gen-elems (cdr els) (+ idx 1) new-cur
                                (append acc el-code store))))))
      (append (gen-elems elements 0 cursor alloc)
              (ldr-offset 0 31 vec-slot)))))

;;; Main code generator: IR -> ARM64 machine code
(defun codegen-expr (ir runtime-addrs fn-offsets current-offset temp-depth)
  (cond
    ;; Literal: load tagged fixnum
    ((has-tag? ir 'lit)
     (let* ((value (car (cdr ir)))
            (tagged (ash value 4)))
       (if (and (>= tagged 0) (< tagged #x10000))
           (movz 0 tagged)
           (load-addr 0 tagged))))

    ;; Variable: load from stack
    ((has-tag? ir 'var)
     (let ((offset (car (cdr ir))))
       (append
        (sub-imm 1 20 (* offset 8))
        (ldr-offset 0 1 0))))

    ;; Set variable: store value to stack
    ((has-tag? ir 'set-var)
     (let* ((offset (car (cdr ir)))
            (val-ir (car (cdr (cdr ir))))
            (val-code (codegen-expr val-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append val-code
               (sub-imm 1 20 (* offset 8))
               (str-offset 0 1 0))))

    ;; String literal
    ((has-tag? ir 'string-lit)
     (codegen-string-from-chars (cdr ir) temp-depth))

    ;; Symbol literal
    ((has-tag? ir 'symbol-lit)
     (let* ((str-code (codegen-string-from-chars (string->char-codes (car (cdr ir))) temp-depth))
            (cursor (if current-offset (+ current-offset (count-instrs str-code)) nil)))
       (append str-code
               (ldr-offset 9 19 88)
               (blr 9))))

    ;; Vector literal
    ((has-tag? ir 'vector-lit)
     (codegen-vector-literal (cdr ir) runtime-addrs fn-offsets current-offset temp-depth))

    ;; Get tag
    ((has-tag? ir 'get-tag)
     (let* ((arg-ir (car (cdr ir)))
            (arg-code (codegen-expr arg-ir runtime-addrs fn-offsets current-offset temp-depth)))
       (append arg-code
               (movz 1 #xF)
               (and-reg 0 0 1)
               (lsl-imm 0 0 4))))

    ;; Captured variable from closure env
    ((has-tag? ir 'capture)
     (let ((idx (car (cdr ir))))
       (append
        (mov-reg 0 24)
        (movz 1 idx)
        (ldr-offset 9 19 72)
        (blr 9))))

    ;; Addition
    ((has-tag? ir 'add)
     (let* ((left-ir (car (cdr ir)))
            (right-ir (car (cdr (cdr ir))))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                              (+ current-offset 1 (count-instrs left-code) 2)
                              nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (str-offset 24 31 x24-slot)
               left-code
               (str-offset 0 31 left-slot)
               (ldr-offset 24 31 x24-slot)
               right-code
               (mov-reg 1 0)
               (ldr-offset 0 31 left-slot)
               (add-reg 0 0 1))))

    ;; Subtraction
    ((has-tag? ir 'sub)
     (let* ((left-ir (car (cdr ir)))
            (right-ir (car (cdr (cdr ir))))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                              (+ current-offset 1 (count-instrs left-code) 2)
                              nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (str-offset 24 31 x24-slot)
               left-code
               (str-offset 0 31 left-slot)
               (ldr-offset 24 31 x24-slot)
               right-code
               (mov-reg 1 0)
               (ldr-offset 0 31 left-slot)
               (sub-reg 0 0 1))))

    ;; Multiplication
    ((has-tag? ir 'mul)
     (let* ((left-ir (car (cdr ir)))
            (right-ir (car (cdr (cdr ir))))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                              (+ current-offset 1 (count-instrs left-code) 2)
                              nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (str-offset 24 31 x24-slot)
               left-code
               (str-offset 0 31 left-slot)
               (ldr-offset 24 31 x24-slot)
               right-code
               (mov-reg 1 0)
               (ldr-offset 0 31 left-slot)
               ;; Untag, multiply, retag
               (lsr-imm 0 0 4)
               (lsr-imm 1 1 4)
               (mul-reg 0 0 1)
               (lsl-imm 0 0 4))))

    ;; Division
    ((has-tag? ir 'div)
     (let* ((left-ir (car (cdr ir)))
            (right-ir (car (cdr (cdr ir))))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                              (+ current-offset 1 (count-instrs left-code) 2)
                              nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (str-offset 24 31 x24-slot)
               left-code
               (str-offset 0 31 left-slot)
               (ldr-offset 24 31 x24-slot)
               right-code
               (mov-reg 1 0)
               (ldr-offset 0 31 left-slot)
               ;; Untag, divide, retag
               (lsr-imm 0 0 4)
               (lsr-imm 1 1 4)
               (sdiv-reg 0 0 1)
               (lsl-imm 0 0 4))))

    ;; Comparison: equal
    ((has-tag? ir 'cmp-eq)
     (let* ((left-ir (car (cdr ir)))
            (right-ir (car (cdr (cdr ir))))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                              (+ current-offset 1 (count-instrs left-code) 2)
                              nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (str-offset 24 31 x24-slot)
               left-code
               (str-offset 0 31 left-slot)
               (ldr-offset 24 31 x24-slot)
               right-code
               (mov-reg 1 0)
               (ldr-offset 0 31 left-slot)
               (cmp-reg 0 1)
               (cset 0 (cond-eq))
               (lsl-imm 0 0 4))))

    ;; Comparison: less than
    ((has-tag? ir 'cmp-lt)
     (let* ((left-ir (car (cdr ir)))
            (right-ir (car (cdr (cdr ir))))
            (x24-slot (temp-slot-offset temp-depth))
            (left-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (left-code (codegen-expr left-ir runtime-addrs fn-offsets
                                     (if current-offset (+ current-offset 1) nil)
                                     nested-depth))
            (right-cursor (if current-offset
                              (+ current-offset 1 (count-instrs left-code) 2)
                              nil))
            (right-code (codegen-expr right-ir runtime-addrs fn-offsets right-cursor nested-depth)))
       (append (str-offset 24 31 x24-slot)
               left-code
               (str-offset 0 31 left-slot)
               (ldr-offset 24 31 x24-slot)
               right-code
               (mov-reg 1 0)
               (ldr-offset 0 31 left-slot)
               (cmp-reg 0 1)
               (cset 0 (cond-lt))
               (lsl-imm 0 0 4))))

    ;; Cons call
    ((has-tag? ir 'cons-call)
     (let* ((car-ir (car (cdr ir)))
            (cdr-ir (car (cdr (cdr ir))))
            (x24-slot (temp-slot-offset temp-depth))
            (car-slot (temp-slot-offset (+ temp-depth 1)))
            (nested-depth (+ temp-depth 2))
            (car-code (codegen-expr car-ir runtime-addrs fn-offsets
                                    (if current-offset (+ current-offset 1) nil)
                                    nested-depth))
            (cdr-cursor (if current-offset
                            (+ current-offset 1 (count-instrs car-code) 2)
                            nil))
            (cdr-code (codegen-expr cdr-ir runtime-addrs fn-offsets cdr-cursor nested-depth)))
       (append (str-offset 24 31 x24-slot)
               car-code
               (str-offset 0 31 car-slot)
               (ldr-offset 24 31 x24-slot)
               cdr-code
               (mov-reg 1 0)
               (ldr-offset 0 31 car-slot)
               (ldr-offset 9 19 0)
               (blr 9))))

    ;; If expression
    ((has-tag? ir 'if-expr)
     (let* ((test-ir (car (cdr ir)))
            (then-ir (car (cdr (cdr ir))))
            (else-ir (car (cdr (cdr (cdr ir)))))
            (test-code (codegen-expr test-ir runtime-addrs fn-offsets current-offset temp-depth))
            (then-base (if current-offset (+ current-offset (count-instrs test-code) 2) nil))
            (then-code (codegen-expr then-ir runtime-addrs fn-offsets then-base temp-depth))
            (else-base (if then-base (+ then-base (count-instrs then-code) 1) nil))
            (else-code (codegen-expr else-ir runtime-addrs fn-offsets else-base temp-depth))
            (then-len (count-instrs then-code))
            (else-len (count-instrs else-code)))
       (append test-code
               (movz 1 0)
               (cmp-reg 0 1)
               (b-cond (cond-eq) (* (+ then-len 1) 4))
               then-code
               (b-offset (* (+ else-len 0) 4))
               else-code)))

    ;; Let expression
    ((has-tag? ir 'let-expr)
     (let* ((bind-values (car (cdr ir)))
            (body-ir (car (cdr (cdr ir))))
            (num-bindings (car (cdr (cdr (cdr ir)))))
            (env-offsets (car (cdr (cdr (cdr (cdr ir))))))
            (x24-slot (temp-slot-offset temp-depth))
            (nested-depth (+ temp-depth 1))
            (cursor (if current-offset (+ current-offset 1) nil))
            (accum (str-offset 24 31 x24-slot)))
       (labels ((gen-bindings (vals offs cur acc)
                  (if (nil? vals)
                      (list acc cur)
                      (let* ((val-ir (car vals))
                             (offset (car offs))
                             (restore (ldr-offset 24 31 x24-slot))
                             (bind-cursor (if cur (+ cur 1) nil))
                             (bind-code (codegen-expr val-ir runtime-addrs fn-offsets bind-cursor nested-depth))
                             (store-code (append
                                          (sub-imm 1 20 (* offset 8))
                                          (str-offset 0 1 0)))
                             (block-instrs (+ 1 (count-instrs bind-code) 2))
                             (new-cur (if cur (+ cur block-instrs) nil)))
                        (gen-bindings (cdr vals) (cdr offs) new-cur
                                      (append acc restore bind-code store-code))))))
         (let* ((result (gen-bindings bind-values env-offsets cursor accum))
                (bind-accum (car result))
                (final-cursor (car (cdr result)))
                (restore (ldr-offset 24 31 x24-slot))
                (body-cursor (if final-cursor (+ final-cursor 1) nil))
                (body-code (codegen-expr body-ir runtime-addrs fn-offsets body-cursor nested-depth)))
           (append bind-accum restore body-code)))))

    ;; Progn
    ((has-tag? ir 'progn)
     (let* ((exprs (cdr ir))
            (x24-slot (temp-slot-offset temp-depth))
            (nested-depth (+ temp-depth 1))
            (cursor (if current-offset (+ current-offset 1) nil))
            (accum (str-offset 24 31 x24-slot)))
       (labels ((gen-exprs (es idx cur acc)
                  (if (nil? es)
                      acc
                      (let* ((restore (if (> idx 0)
                                          (ldr-offset 24 31 x24-slot)
                                          nil))
                             (chunk-cursor (if (and cur (> idx 0)) (+ cur 1) cur))
                             (chunk (codegen-expr (car es) runtime-addrs fn-offsets chunk-cursor nested-depth))
                             (instrs (+ (count-instrs restore) (count-instrs chunk)))
                             (new-cur (if cur (+ cur instrs) nil)))
                        (gen-exprs (cdr es) (+ idx 1) new-cur
                                   (append acc restore chunk))))))
         (gen-exprs exprs 0 cursor accum))))

    ;; Default: return 0
    (t (movz 0 0))))

;;; Generate prologue for function
(defun codegen-prologue ()
  (append
   (stp-offset 29 30 31 (- *stack-frame-size*))
   (sub-imm 31 31 *stack-frame-size*)
   (mov-reg 29 31)
   (stp-offset 19 20 31 16)
   (stp-offset 21 22 31 32)
   (stp-offset 23 24 31 48)
   (mov-reg 20 31)
   (add-imm 20 20 *env-base-offset*)))

;;; Generate epilogue for function
(defun codegen-epilogue ()
  (append
   (ldp-offset 23 24 31 48)
   (ldp-offset 21 22 31 32)
   (ldp-offset 19 20 31 16)
   (add-imm 31 31 *stack-frame-size*)
   (ldp-offset 29 30 31 0)
   (ret)))

;;; Generate main function wrapper
(defun codegen-main-with-runtime (ir runtime-addrs)
  (let* ((body-code (codegen-expr ir runtime-addrs nil 20 0)))
    (append (codegen-prologue)
            body-code
            (codegen-epilogue))))
