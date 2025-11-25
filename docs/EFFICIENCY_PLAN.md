# Habu Compiler Efficiency Improvement Plan

**Created**: November 25, 2025
**Goal**: Achieve self-hosting ASAP with hybrid symbol interning approach
**Strategy**: Fix critical blockers first, optimize later

---

## Executive Summary

The Habu ARM64 Lisp compiler has all functional prerequisites for self-hosting but is blocked by **missing symbol interning**. Additional issues include critical codegen bugs and O(N²) algorithms that slow compilation.

**Critical Finding**: `(eq 'foo 'foo)` returns false because each symbol literal creates a fresh object.

**Strategy**: Hybrid approach - implement simple C-based symbol table immediately, migrate to full Lisp implementation post-self-hosting.

---

## Week 1: Critical Path to Self-Hosting

### Priority 0: Symbol Interning (Days 1-2, 8 hours)

**Problem**: `habu_make_symbol` in runtime/gc.c:1196 allocates fresh symbol on every call.

**Current Code** (runtime/gc.c:1196-1218):
```c
habu_value_t habu_make_symbol(const char *name) {
    void *mem = habu_gc_alloc(sizeof(habu_symbol_t), TYPE_SYMBOL);
    habu_symbol_t *sym = (habu_symbol_t *)mem;
    sym->name = habu_make_string(name, strlen(name));
    return tag_pointer(sym, TAG_SYMBOL);
    // BUG: No interning! Every call creates new symbol
}
```

**Solution**: Add hash table interning

```c
// Add to runtime/gc.c
#define SYMBOL_TABLE_SIZE 1024

typedef struct symbol_entry {
    char *name;
    habu_value_t symbol;
    struct symbol_entry *next;
} symbol_entry_t;

static symbol_entry_t *symbol_table[SYMBOL_TABLE_SIZE];

static unsigned hash_string(const char *str) {
    unsigned hash = 5381;
    while (*str) hash = ((hash << 5) + hash) + *str++;
    return hash % SYMBOL_TABLE_SIZE;
}

habu_value_t habu_make_symbol(const char *name) {
    unsigned idx = hash_string(name);

    // Check for existing symbol
    for (symbol_entry_t *e = symbol_table[idx]; e; e = e->next) {
        if (strcmp(e->name, name) == 0) {
            return e->symbol;  // Return cached symbol
        }
    }

    // Not found, create new symbol
    void *mem = habu_gc_alloc(sizeof(habu_symbol_t), TYPE_SYMBOL);
    habu_symbol_t *sym = (habu_symbol_t *)mem;
    sym->name = habu_make_string(name, strlen(name));
    habu_value_t result = tag_pointer(sym, TAG_SYMBOL);

    // Add to table
    symbol_entry_t *entry = malloc(sizeof(symbol_entry_t));
    entry->name = strdup(name);
    entry->symbol = result;
    entry->next = symbol_table[idx];
    symbol_table[idx] = entry;

    return result;
}
```

**Test Validation**:
```lisp
(eq 'foo 'foo)                    ; => 1 (currently returns 0)
(eq (car (cons 'foo 2)) 'foo)     ; => 1
(eq 'bar 'baz)                    ; => 0
```

**Effort**: 4 hours implementation + 4 hours testing
**Impact**: UNBLOCKS self-hosting

**Migration Path**: Once self-hosting works, migrate to full runtime/symbols.lisp:135-153 implementation with package support.

---

### Priority 1: Critical Bug Fixes (Days 3-4, 6 hours)

#### 1.1 Missing arm64-sub-imm Function (1 hour)

**Problem**: Called at habu-arm64-codegen-sbcl.lisp:932, but undefined → runtime crash

**Solution**: Add after line 220:
```lisp
(defun arm64-sub-imm (rd rn imm)
  "SUB Rd, Rn, #imm - subtract immediate from register"
  (let* ((sf 1)        ; 64-bit operation
         (op 1)        ; SUB (not ADD)
         (s 0)         ; Don't set flags
         (sh 0)        ; No shift
         (imm12 (logand imm #xFFF))
         (instr (logior (ash sf 31)
                       (ash op 30)
                       (ash s 29)
                       #x11000000  ; ADD/SUB immediate base
                       (ash sh 22)
                       (ash imm12 10)
                       (ash rn 5)
                       rd)))
    (list (logand instr #xFF)
          (logand (ash instr -8) #xFF)
          (logand (ash instr -16) #xFF)
          (logand (ash instr -24) #xFF))))
```

**Test**: Any function with stack frame allocation

#### 1.2 Hardcoded Instruction Functions (5 hours)

**Problem**: arm64-str/ldr/stp/ldp (lines 141-210) only work for specific hardcoded register combinations, return (0 0 0 0) otherwise.

**Current Code** (habu-arm64-codegen-sbcl.lisp:141-170):
```lisp
(defun arm64-str (rt rn imm)
  (if (= rt 0)
    (if (= rn 31)  ; sp
      (if (= imm -16)
        (quote (240 15 31 248))  ; Hardcoded bytes
        (quote (0 0 0 0)))       ; FAILS silently!
      (quote (0 0 0 0)))
    (quote (0 0 0 0))))          ; FAILS for any other register!
```

**Solution**: Implement proper encoding:
```lisp
(defun arm64-str (rt rn imm)
  "STR Rt, [Rn, #imm] - store register to memory"
  (let* ((size 3)     ; 64-bit
         (v 0)        ; Not SIMD
         (opc 0)      ; STR
         (imm12 (ash imm -3))  ; Scale by 8 for 64-bit
         (instr (logior (ash size 30)
                       (ash #b111 27)
                       (ash v 26)
                       (ash #b01 24)
                       (ash opc 22)
                       (ash (logand imm12 #xFFF) 10)
                       (ash rn 5)
                       rt)))
    (list (logand instr #xFF)
          (logand (ash instr -8) #xFF)
          (logand (ash instr -16) #xFF)
          (logand (ash instr -24) #xFF))))

(defun arm64-ldr (rt rn imm)
  "LDR Rt, [Rn, #imm] - load register from memory"
  (let* ((size 3)     ; 64-bit
         (v 0)        ; Not SIMD
         (opc 1)      ; LDR
         (imm12 (ash imm -3))  ; Scale by 8
         (instr (logior (ash size 30)
                       (ash #b111 27)
                       (ash v 26)
                       (ash #b01 24)
                       (ash opc 22)
                       (ash (logand imm12 #xFFF) 10)
                       (ash rn 5)
                       rt)))
    (list (logand instr #xFF)
          (logand (ash instr -8) #xFF)
          (logand (ash instr -16) #xFF)
          (logand (ash instr -24) #xFF))))

(defun arm64-stp (rt1 rt2 rn imm)
  "STP Rt1, Rt2, [Rn, #imm] - store pair of registers"
  (let* ((opc 2)      ; 64-bit
         (v 0)        ; Not SIMD
         (l 0)        ; Store (not load)
         (imm7 (ash imm -3))  ; Scale by 8
         (instr (logior (ash opc 30)
                       (ash #b101 27)
                       (ash v 26)
                       (ash #b0 25)
                       (ash #b10 23)  ; Signed offset
                       (ash l 22)
                       (ash (logand imm7 #x7F) 15)
                       (ash rt2 10)
                       (ash rn 5)
                       rt1)))
    (list (logand instr #xFF)
          (logand (ash instr -8) #xFF)
          (logand (ash instr -16) #xFF)
          (logand (ash instr -24) #xFF))))

(defun arm64-ldp (rt1 rt2 rn imm)
  "LDP Rt1, Rt2, [Rn, #imm] - load pair of registers"
  (let* ((opc 2)      ; 64-bit
         (v 0)        ; Not SIMD
         (l 1)        ; Load (not store)
         (imm7 (ash imm -3))  ; Scale by 8
         (instr (logior (ash opc 30)
                       (ash #b101 27)
                       (ash v 26)
                       (ash #b0 25)
                       (ash #b10 23)  ; Signed offset
                       (ash l 22)
                       (ash (logand imm7 #x7F) 15)
                       (ash rt2 10)
                       (ash rn 5)
                       rt1)))
    (list (logand instr #xFF)
          (logand (ash instr -8) #xFF)
          (logand (ash instr -16) #xFF)
          (logand (ash instr -24) #xFF))))
```

**Test**: Stack operations, function prologue/epilogue

---

### Priority 2: Essential Quick Wins (Day 5, 12 hours)

#### 2.1 Fix O(N²) Free Variable Analysis (3 hours)

**Location**: bootstrap/compiler.lisp:608-612

**Current Code**:
```lisp
(defun find-free-variables (body params &optional (env nil))
  (let* ((all-vars (collect-variables body))      ; O(N) traversal
         (bound-vars (append params env)))        ; O(N) allocation
    (remove-duplicates                            ; O(N²)!
      (set-difference all-vars bound-vars))))     ; O(N×M)
```

**Solution**: Single-pass algorithm with hash set:
```lisp
(defun find-free-variables (body params &optional (env nil))
  (let ((bound-set (make-hash-table :test #'eq)))
    ;; Mark all bound variables
    (dolist (var params) (setf (gethash var bound-set) t))
    (dolist (var env) (setf (gethash var bound-set) t))
    ;; Single-pass collection of free variables
    (let ((free-vars nil)
          (seen (make-hash-table :test #'eq)))
      (labels ((collect (expr)
                 (cond
                   ((symbolp expr)
                    (when (and (not (gethash expr bound-set))
                              (not (gethash expr seen))
                              (not (keywordp expr)))
                      (setf (gethash expr seen) t)
                      (push expr free-vars)))
                   ((consp expr)
                    (collect (car expr))
                    (collect (cdr expr))))))
        (collect body)
        free-vars))))
```

**Impact**: O(N) instead of O(N²), 5-10x faster for large functions

#### 2.2 Optimize Excessive Append Usage (7 hours)

**Problem**: 50+ append calls during code generation, each O(N) copy

**Locations**:
- habu-arm64-codegen-sbcl.lisp:550-851 (binary ops)
- habu-arm64-codegen-sbcl.lisp:1200-1500 (control flow)

**Current Pattern**:
```lisp
(append (emit-expr left env)
        (push-instruction)
        (emit-expr right env)
        (pop-and-op-instruction))
```

**Solution**: Accumulator pattern with nconc:
```lisp
(let ((code nil))
  (setf code (nconc (emit-expr left env) code))
  (setf code (nconc (push-instruction) code))
  (setf code (nconc (emit-expr right env) code))
  (setf code (nconc (pop-and-op-instruction) code))
  (nreverse code))
```

Or use a code accumulator object:
```lisp
(defstruct code-acc
  (head nil)
  (tail nil))

(defun acc-emit (acc instructions)
  (if (code-acc-tail acc)
      (setf (cdr (code-acc-tail acc)) instructions
            (code-acc-tail acc) (last instructions))
      (setf (code-acc-head acc) instructions
            (code-acc-tail acc) (last instructions))))

(defun acc-result (acc)
  (code-acc-head acc))
```

**Impact**: 30-50% reduction in codegen allocations

#### 2.3 Cache Let Binding Parses (2 hours)

**Location**: habu-arm64-codegen-sbcl.lisp:2560-2569

**Problem**: Let* bindings parsed twice - once to collect, once to emit

**Current Flow**:
1. Parse bindings to count variables → O(N)
2. Parse bindings again to emit code → O(N) (redundant!)

**Solution**: Parse once, cache IR, emit from cached IR:
```lisp
(defun emit-let* (bindings body env)
  ;; Parse all bindings once
  (let ((parsed-bindings
          (mapcar (lambda (b)
                    (list (car b) (parse-expr (cadr b) env)))
                  bindings)))
    ;; Emit from cached parses
    (emit-let*-from-parsed parsed-bindings body env)))
```

**Impact**: 2x faster for let-heavy code

---

## Week 2: Self-Hosting Attempt

### Stage 0: SBCL-Hosted Compilation
```bash
sbcl --load habu-arm64-codegen-sbcl.lisp \
     --eval "(compile-to-file \"habu-arm64-codegen-sbcl.lisp\" \"habu-stage1\")"
```

### Stage 1: Self-Hosted Compilation
```bash
./habu-stage1 habu-arm64-codegen-sbcl.lisp > habu-stage2
```

### Stage 2: Verify Fixed Point
```bash
diff habu-stage1 habu-stage2
# Exit code 0 = SUCCESS (byte-identical)
```

### Debugging Strategy

If self-hosting fails:
1. Identify which function causes divergence
2. Compare IR output between SBCL and Habu versions
3. Add diagnostic prints to both compilers
4. Isolate minimal failing case
5. Fix and iterate

---

## Deferred Improvements (Post Self-Hosting)

### Phase 2: Code Quality (2-3 weeks)

#### 2.1 Peephole Optimization (1 week)

Patterns to optimize:
```
;; Redundant push/pop
push x0; pop x0  →  (nothing)

;; Constant folding
mov x0, #5; mov x1, #3; add x0, x0, x1  →  mov x0, #8

;; Dead stores
str x0, [sp, #64]; ldr x0, [sp, #64]  →  (nothing)

;; Branch optimization
b.eq L1; b L2; L1:  →  b.ne L2
```

**Impact**: 20% code size reduction, 10-15% performance

#### 2.2 Register Allocation (1 week)

Currently unused: x5-x15 (11 registers!)

Strategy:
1. Build interference graph from IR
2. Graph coloring for register assignment
3. Spill to stack only when necessary
4. Prioritize hot variables (loop counters, accumulators)

**Impact**: 40-60% performance improvement

#### 2.3 Calling Convention Improvements (3 days)

- Tail call optimization for recursive functions
- Register-based varargs instead of stack
- Inline small functions

**Impact**: 20-30% performance for recursive code

### Phase 3: Architecture (3-5 weeks)

#### 3.1 Hash-Based Environments (1 week)

Replace linear list with hash table for O(1) lookups:
```lisp
;; Current: O(N) lookup
(assoc 'var env)

;; Improved: O(1) lookup
(gethash 'var env-table)
```

**Impact**: 2-3x compilation speedup

#### 3.2 Unified IR Backend (2 weeks)

Create shared IR layer:
```
Source → Parse → IR → Optimize → ARM64/x86_64
```

Eliminate 2500+ lines of duplication between backends.

**Impact**: Easier maintenance, new backends possible

#### 3.3 Full Lisp Symbol Table (1 week)

Migrate from C hash table to runtime/symbols.lisp:135-153:
- Package support (intern "FOO" :pkg)
- symbol-package, symbol-name accessors
- Export/import lists

**Impact**: Full CL compatibility

---

## Success Metrics

### Week 1 Targets
- [ ] `(eq 'foo 'foo)` returns true
- [ ] All 90+ existing tests pass
- [ ] No crashes from arm64-sub-imm
- [ ] No silent failures from hardcoded instructions

### Week 2 Targets
- [ ] Stage 0 → Stage 1 compilation succeeds
- [ ] Stage 1 → Stage 2 compilation succeeds
- [ ] Stage 1 == Stage 2 (fixed point achieved)

### Performance Targets
- Compilation time: < 5 seconds for 7500-line compiler
- Generated code: < 100KB binary
- Test suite: < 10 seconds for all 90+ tests

---

## Risk Mitigation

**Risk 1**: Symbol interning introduces GC bugs
- **Mitigation**: Root symbol table properly, extensive testing
- **Fallback**: Simple linear list initially, optimize to hash later

**Risk 2**: Hardcoded instruction fixes break existing code
- **Mitigation**: Run full test suite before/after
- **Fallback**: Keep hardcoded paths as special cases

**Risk 3**: Self-hosting reveals missing features
- **Mitigation**: Test suite covers compiler patterns
- **Fallback**: Add stubs for missing functions

**Risk 4**: O(N²) fixes introduce bugs
- **Mitigation**: Compare output before/after on test cases
- **Fallback**: Keep old code behind feature flag

---

## Files to Modify

| File | Changes | Priority |
|------|---------|----------|
| runtime/gc.c | Add symbol interning hash table | P0 |
| habu-arm64-codegen-sbcl.lisp:220 | Add arm64-sub-imm | P1 |
| habu-arm64-codegen-sbcl.lisp:141-210 | Parametrize arm64-str/ldr/stp/ldp | P1 |
| bootstrap/compiler.lisp:608-612 | Fix O(N²) free variable analysis | P2 |
| habu-arm64-codegen-sbcl.lisp:550-851 | Optimize append usage | P2 |

## Test Files to Create

| File | Purpose |
|------|---------|
| tests/test_symbol_interning.lisp | Verify eq on symbols |
| tests/test_compiler_patterns.lisp | Test compiler helper functions |
| tests/test_self_hosting.lisp | End-to-end self-hosting test |

---

**Status**: Plan ready for implementation
**Next Action**: Implement symbol interning in runtime/gc.c
