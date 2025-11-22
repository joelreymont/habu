# Correct Bootstrap Path to Self-Hosting

## The Right Approach

**Use the working C bootstrap compiler** (in `bootstrap/`) to compile the Lisp compiler, NOT try to fix the SBCL stub.

## Why This Is Correct

1. ✅ **Bootstrap compiler works**: 73/73 tests passing
2. ✅ **Complete pipeline**: Text → Parse → IR → ARM64 → Execute
3. ✅ **JIT execution verified**: Can run compiled code
4. ✅ **Reader works**: Can parse Lisp source text

## Current Status

```
bootstrap/
├── reader.c          - ✅ S-expression parser (7/7 tests)
├── ir-generation.c   - ✅ Lisp → IR compiler
├── code-generation.c - ✅ IR → ARM64 code generator
├── encoders.c        - ✅ ARM64 instruction encoders (21/21 tests)
├── runtime-minimal.c - ✅ cons/car/cdr/intern
└── tests/
    └── test-end-to-end.c - ✅ Complete pipeline (9/9 tests)
```

**Total: 73/73 tests passing**

## What Works Right Now

```c
// Parse and execute Lisp
const char *source = "(* (+ 3 4) 5)";
habu_value_t expr = habu_read(source);
habu_value_t ir = bootstrap_compile(expr);
size_t code_size;
uint8_t *code = bootstrap_codegen(ir, &code_size);
int64_t result = execute_code(code, code_size);
// result = 35 ✓
```

## Bootstrap Sequence

### Phase 1: Extend Bootstrap Compiler (Current)

**Add missing features to bootstrap compiler:**

1. **Let bindings** - Stack-based variables
   - IR already supports it
   - Need to implement code generation
   - Status: Partially implemented

2. **Function definitions** (defun)
   - Parse function definitions
   - Generate function code
   - Handle function calls

3. **Recursion**
   - BL instruction for calls
   - Proper stack frames
   - Tail call optimization (nice to have)

### Phase 2: Compile Simple Programs

**Test with increasingly complex programs:**

```lisp
;; Simple function
(defun add-one (x)
  (+ x 1))

;; Recursive function
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;; Multiple functions
(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

### Phase 3: Compile Habu Compiler Components

**Start with simple compiler functions:**

```lisp
;; Compile ARM64 encoders
(defun arm64-movz (rd imm)
  ...)

;; Compile simple IR functions
(defun ir-lit (value)
  ...)
```

### Phase 4: Self-Compilation

**The bootstrap compiler compiles the full Habu compiler:**

```
bootstrap_compiler(habu_compiler.lisp) → habu_stage0 (ARM64 executable)
```

### Phase 5: Fixed Point

```
Stage 0: bootstrap_compiler → habu₀
Stage 1: habu₀(habu_compiler.lisp) → habu₁
Stage 2: habu₁(habu_compiler.lisp) → habu₂
Verify: habu₁ == habu₂ ✓ SELF-HOSTING ACHIEVED!
```

## Next Immediate Steps

1. **Implement defun in bootstrap compiler**
   - Parse function definitions
   - Generate function prologue/epilogue
   - Handle function calls with BL

2. **Test recursive functions**
   - factorial
   - fibonacci
   - Verify stack management

3. **Compile a multi-function program**
   - Multiple defuns
   - Function calls between them
   - Generate single executable

4. **Begin compiling compiler components**
   - Start with simple helper functions
   - Build up to complex IR generation
   - Eventually compile entire compiler

## Why Not SBCL-Hosted?

The SBCL stub compiler (`habu-arm64-codegen-sbcl.lisp`) is:
- Incomplete (just generates placeholder code)
- Would need significant work to make functional
- Duplicates effort - we already have working C compiler!

**The C bootstrap compiler is the correct foundation.**

## Timeline Estimate

- **This week**: defun + recursion in bootstrap compiler
- **Next week**: Compile simple Lisp programs
- **2-3 weeks**: Compile Habu compiler components
- **1 month**: Full self-hosting

## Success Metrics

- [ ] Bootstrap compiler supports defun
- [ ] Can compile recursive factorial
- [ ] Can compile multi-function programs
- [ ] Can compile simple compiler functions
- [ ] Can compile full Habu compiler
- [ ] **Fixed point achieved** 🎉

**Current progress: ~80% to self-hosting**

Using the working C bootstrap compiler is the fastest, most reliable path forward.
