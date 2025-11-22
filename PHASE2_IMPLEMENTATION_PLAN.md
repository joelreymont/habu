# Phase 2 Implementation Plan - Enhanced Language Features

## Current Implementation Review

### Strengths ✅
1. **Working Foundation**: Core arithmetic, conditionals, and runtime calls functional
2. **Proper ABI**: ARM64 calling convention implemented correctly
3. **Runtime Integration**: Function table pattern solves ASLR issues
4. **Clean IR Design**: Clear separation between IR generation and code generation

### Critical Issues to Address 🔴

#### 1. Performance Issues
- **Runtime Table Lookups**: Loading function pointers on every call (LDR + BLR)
  - *Solution*: Cache frequently used functions in registers
- **No Register Allocation**: All values go through x0
  - *Solution*: Basic register allocator for temporaries (x0-x7)
- **Stack Operations**: Unoptimized prologue/epilogue
  - *Solution*: Only save registers that are actually used

#### 2. Correctness Issues
- **Stack Alignment**: Not guaranteed 16-byte aligned for all call paths
- **Missing Operators**: Only have `=`, need `<`, `>`, `<=`, `>=`, `!=`
- **No Type Checking**: Can crash on type errors
- **Limited Error Handling**: No bounds checking or overflow detection

#### 3. Architectural Limitations
- **Hardcoded Runtime Table**: Only 3 functions (cons, car, cdr)
- **No User Functions**: Can't define or call user functions
- **No Local Variables**: env-lookup stub returns nil
- **No Stack Frames**: Can't implement let or function calls properly

## Phase 2 Implementation Plan

### Stage 2.1: Foundation Improvements (Day 1)

#### A. Extended Runtime Table
```c
// run-bytecode.c
typedef struct {
    void* cons;     // 0
    void* car;      // 8
    void* cdr;      // 16
    void* eq;       // 24
    void* lt;       // 32
    void* add;      // 40
    void* sub;      // 48
    void* mul;      // 56
    void* div;      // 64
    // ... extensible
} runtime_table_t;
```

#### B. Register Allocation Strategy
```lisp
;; Callee-saved: x19-x28 (preserve across calls)
;; x19: runtime table (already implemented)
;; x20: current environment pointer
;; x21-x23: reserved for let-bound variables
;; x24-x28: available for optimization

;; Caller-saved: x0-x18 (can be clobbered)
;; x0-x7: argument/result registers
;; x8-x15: temporary registers
;; x16-x17: intra-procedure call temps
;; x18: platform reserved
```

#### C. Comparison Operators Implementation
```lisp
;; Add to compile-expr
((eq op '<)  (list 'cmp-lt left right))
((eq op '>)  (list 'cmp-gt left right))
((eq op '<=) (list 'cmp-le left right))
((eq op '>=) (list 'cmp-ge left right))
((eq op '!=) (list 'cmp-ne left right))

;; ARM64 conditions: EQ=0, NE=1, LT=11, GE=10, GT=12, LE=13
```

### Stage 2.2: Stack-Based Let Bindings (Day 2-3)

#### Stack Frame Layout
```
    High Address
    +----------------+
    | saved x30 (LR) |  [sp + 24]
    | saved x29 (FP) |  [sp + 16]
    | saved x19      |  [sp + 8]
    | saved x20      |  [sp + 0]   <- SP after prologue
    +----------------+
    | let var 1      |  [sp - 8]
    | let var 2      |  [sp - 16]
    | ...            |
    +----------------+
    Low Address
```

#### Implementation
```lisp
;; IR for let: (let-bind ((x 10) (y 20)) body)
(defun compile-let (bindings body env)
  (let* ((new-env (extend-env bindings env))
         (bind-irs (mapcar #'compile-binding bindings))
         (body-ir (compile-expr body new-env)))
    (list 'let-expr bind-irs body-ir (length bindings))))

;; Codegen: allocate stack space, store values
(defun codegen-let (bind-codes body-code num-bindings)
  (append
    (arm64-sub-imm 31 31 (* 8 num-bindings))  ; Allocate
    (store-bindings bind-codes)                ; Store each
    body-code                                   ; Execute body
    (arm64-add-imm 31 31 (* 8 num-bindings)))) ; Deallocate
```

### Stage 2.3: Function Definition & Calls (Day 4-5)

#### Function Representation
```lisp
;; IR: (defun-ir name params body env)
;; Compiled: (fn-entry name param-count local-count body-code)

(defstruct function
  name
  params
  body
  env           ; Lexical environment at definition
  entry-point)  ; Offset in code segment
```

#### Calling Convention
```
;; ARM64 Function Call ABI
;; Arguments: x0-x7 (up to 8 args)
;; Return: x0
;; Callee-saved: x19-x28, must preserve
;; Stack: 16-byte aligned before BL/BLR
```

#### Implementation
```lisp
;; Function call IR: (call-fn fn-name args)
(defun compile-call (fn-name args env fenv)
  (let ((fn (lookup-function fn-name fenv))
        (arg-irs (mapcar (lambda (a) (compile-expr a env fenv)) args)))
    (list 'call-fn fn arg-irs)))

;; Codegen: Load args to x0-x7, call function
(defun codegen-call (fn arg-codes)
  (append
    (load-arguments arg-codes)      ; Move to x0-x7
    (arm64-bl (fn-entry-point fn))  ; Direct call
    ))                               ; Result in x0
```

### Stage 2.4: Closures (Day 6-7)

#### Closure Structure
```lisp
;; Runtime representation (cons-based)
;; Closure: (tag . (code-ptr . captured-env))
;; tag = 3 (closure tag)

(defun make-closure (code-ptr env)
  (cons 3 (cons code-ptr env)))

;; IR: (closure params body free-vars)
;; Free vars captured at creation time
```

### Performance Optimizations

#### 1. Register Caching
```lisp
;; Cache common runtime functions in registers
(defun setup-runtime-cache ()
  (append
    (arm64-ldr 24 19 0)   ; x24 = cons
    (arm64-ldr 25 19 8)   ; x25 = car
    (arm64-ldr 26 19 16))) ; x26 = cdr

;; Use cached: BLR x24 instead of LDR+BLR
```

#### 2. Peephole Optimizations
```lisp
;; Pattern: (mov x1 x0) (mov x0 x2) (mov x2 x1)
;; Optimize to: (mov x1 x0) (mov x0 x2) (mov x2 x1)
;; Better: Use parallel moves or different registers

(defun optimize-moves (code)
  ;; Detect and eliminate redundant moves
  )
```

#### 3. Tail Call Optimization
```lisp
;; Detect tail position calls
;; Instead of BL + RET, use B (branch)
(defun is-tail-position? (expr)
  ;; Check if expr is in tail position
  )
```

## Implementation Schedule

### Week 1
- **Day 1**: Foundation improvements (runtime table, registers)
- **Day 2-3**: Let bindings with stack frames
- **Day 4-5**: Function definition and calls
- **Weekend**: Testing and debugging

### Week 2
- **Day 6-7**: Closures with environment capture
- **Day 8**: Performance optimizations
- **Day 9**: Comprehensive testing
- **Day 10**: Begin self-compilation attempt

## Test Suite Requirements

### Unit Tests
```lisp
;; test-let.lisp
(let ((x 10)) x) => 10
(let ((x 10) (y 20)) (+ x y)) => 30
(let ((x 10)) (let ((y 20)) (+ x y))) => 30

;; test-defun.lisp
(defun add (x y) (+ x y))
(add 5 7) => 12

;; test-closure.lisp
(defun make-adder (n) (lambda (x) (+ x n)))
((make-adder 10) 5) => 15
```

### Integration Tests
- Factorial (recursive)
- Fibonacci (recursive and iterative)
- List operations (map, filter, reduce)
- Mutual recursion

## Success Metrics

1. **Correctness**: All test cases pass
2. **Performance**:
   - Function calls < 20 instructions overhead
   - Let bindings < 5 instructions per binding
3. **Memory**: Stack usage < 1KB for typical programs
4. **Self-hosting**: Compiler can compile itself

## Risk Mitigation

### Risk 1: Stack Overflow
- **Mitigation**: Add stack limit checks
- **Fallback**: Increase stack size in runtime

### Risk 2: Register Allocation Bugs
- **Mitigation**: Conservative approach, always save/restore
- **Fallback**: Use stack for all temporaries

### Risk 3: Closure Complexity
- **Mitigation**: Start with simple flat closures
- **Fallback**: Defer to Phase 3 if needed

## Next Steps

Begin with Stage 2.1: Foundation Improvements
1. Extend runtime table structure
2. Implement comparison operators
3. Set up register allocation framework