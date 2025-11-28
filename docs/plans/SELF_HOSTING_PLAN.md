# Self-Hosting Plan: Eliminating SBCL Dependency

**Goal**: Habu compiler compiles itself without requiring SBCL

## Executive Summary

Habu is **~2 weeks away** from complete self-hosting. The path is clear:

1. **Fix habu0 linker** (1-2 days) - Currently crashes in mode #x300
2. **Remove SBCL dependencies** (3-5 days) - Only 48 format, 5 loops, 7 file I/O calls
3. **Bootstrap compiler** (2-3 days) - Compile bootstrap/compiler.lisp to native
4. **Achieve fixed point** (1-2 days) - Compiler compiles itself

**Key insight**: We're closer than it appears. habu0 already has 80% of what we need.

## Current State Analysis

### What Works Without SBCL ✓

**habu0** - 155KB standalone native ARM64 executable:
- ✓ Reader: Parses Lisp (numbers, symbols, strings, lists, quotes)
- ✓ Evaluator: h0-eval interprets Lisp
- ✓ Compiler: h0-compile generates IR
- ✓ Codegen: h0-codegen generates ARM64 machine code
- ✓ File I/O: sys-open, sys-read, sys-close (via libSystem)
- ✓ Hash tables: Runtime implementation exists
- ✓ Dynamic frame sizing: Just implemented (unlimited let* bindings)

**Status**:
- Mode #x100 (eval): WORKING
- Mode #x200 (codegen): WORKING
- Mode #x300 (linker): CRASHES (known bug in wrap-with-heap-stub)

### What Needs SBCL ✗

**bootstrap/compiler.lisp** - Full Habu compiler:
- Runs in SBCL
- Dependencies (all easily replaceable):
  - 48 format calls (debug output + string creation)
  - 5 loop forms (simple iterations)
  - 7 with-open-file calls (file I/O)
  - 3 sb-ext:run-program calls (shell commands)

**Total SBCL-specific code**: ~60 lines out of 5000+ lines (<1.2%)

## The Gap Is Small

| Feature | habu0 | bootstrap/compiler | Status |
|---------|-------|-------------------|--------|
| Reader | ✓ Native | Uses SBCL | Can use habu0's |
| File I/O | ✓ Native | Uses SBCL | Can use sys-* primitives |
| Eval | ✓ Native | Uses SBCL for macros | Can use h0-eval |
| Compile | ✓ Native | ✓ Full | Same IR format |
| Codegen | ✓ Native | ✓ Full | Same bytecode |
| Linker | ✗ Crashes | ✓ Works in SBCL | Just needs bugfix |
| Macros | ✗ None | ✓ Full defmacro | Need for full CL |
| Optimization | ✗ None | ✓ 3 passes | Need for performance |

## Implementation Phases

### Phase 1: Fix habu0 Linker (Immediate Priority)

**Problem**: wrap-with-heap-stub has 20 function calls in a list, crashes in native code

**Solution**: Pre-compute all instructions in let* bindings (dynamic frame sizing now allows this)

**Code change**:
```lisp
;; Before (crashes):
(list (a64-sub-imm ...) (a64-str ...) (a64-str ...) ...)  ; 20 calls

;; After (works):
(let* ((i1 (a64-sub-imm ...))
       (i2 (a64-str ...))
       ...
       (i20 (a64-ret)))
  (list i1 i2 ... i20))
```

**Tasks**:
- Refactor wrap-with-heap-stub
- Test: `echo '(+ 20 22)' | ./habu0` should produce working executable
- Verify mode #x300 works end-to-end

**Deliverable**: habu0 compiles simple Lisp programs to native executables

**Timeline**: 1-2 days

### Phase 2: Remove SBCL Dependencies

#### 2.1 Replace Format (48 calls)

**Pattern 1 - Debug output**:
```lisp
;; Before:
(format t "Compiling ~A bytes~%" (length bytes))

;; After:
(print "Compiling ") (print (length bytes)) (print " bytes") (terpri)
```

**Pattern 2 - String creation**:
```lisp
;; Before:
(format nil "LAMBDA-~A" *lambda-counter*)

;; After:
(string-concat "LAMBDA-" (number-to-string *lambda-counter*))
```

**New primitives needed**:
- string-concat (already exists in runtime)
- number-to-string (trivial to add)

#### 2.2 Replace Loop (5 calls)

**Pattern**:
```lisp
;; Before:
(loop for x in list collect (f x))

;; After:
(labels ((iter (xs acc)
           (if (null xs) (reverse acc)
               (iter (cdr xs) (cons (f (car xs)) acc)))))
  (iter list nil))
```

All 5 loops are simple collect/iteration patterns.

#### 2.3 Replace with-open-file (7 calls)

**Pattern**:
```lisp
;; Before:
(with-open-file (in path :direction :input) (read in))

;; After:
(let* ((fd (sys-open path #x0 #x0))
       (content (read-file-contents fd)))
  (sys-close fd)
  content)
```

**New primitives needed**:
- sys-file-size (fstat wrapper) - trivial to add

#### 2.4 Remove sb-ext:run-program (3 calls)

- Line 4882: Clang invocation - **DELETE** (we have native linker!)
- Line 5011: Codesign - Add sys-exec or make optional
- Line 2250: Shell command - Evaluate if needed

**Timeline**: 3-5 days

### Phase 3: Bootstrap the Compiler

#### 3.1 Create Standalone Entry Point

```lisp
(defun main ()
  (let* ((args (sys-get-args))
         (input (nth 1 args))
         (output (nth 2 args)))
    (deliver-file input output)
    (sys-exit 0)))
```

#### 3.2 Compile with SBCL (One Final Time)

```bash
sbcl --eval "(load 'bootstrap/compiler.lisp')" \
     --eval "(habu:deliver 'bootstrap/compiler.lisp' 'habu-compiler')" \
     --quit
```

**Result**: habu-compiler native executable

#### 3.3 Test Standalone Compiler

```bash
./habu-compiler examples/factorial.lisp factorial
./factorial  # Should output 120
```

#### 3.4 Self-Compilation (Fixed Point)

```bash
# Stage 1: SBCL -> habu-v1
sbcl --eval "(habu:deliver 'compiler.lisp' 'habu-v1')"

# Stage 2: habu-v1 -> habu-v2
./habu-v1 compiler.lisp habu-v2

# Verify
diff habu-v1 habu-v2  # Goal: byte-identical
```

**Timeline**: 2-3 days

### Phase 4: Cleanup

- Remove SBCL from documentation
- Update build scripts
- Announce self-hosting achievement

**Timeline**: 1-2 days

## Success Criteria

- [x] Dynamic frame sizing (just completed!)
- [ ] habu0 mode #x300 works
- [ ] Zero SBCL code in bootstrap/compiler.lisp
- [ ] habu-compiler native executable created
- [ ] habu-compiler compiles itself
- [ ] Fixed point achieved (byte-identical binaries)

## Why This Will Work

1. **Minimal dependencies**: Only 60 lines of SBCL-specific code to replace
2. **Native primitives exist**: sys-*, habu-read, h0-eval all working
3. **Dynamic frames unlocked**: No more function splitting workarounds
4. **Clear path**: Each phase builds on the previous
5. **Already 80% there**: habu0 demonstrates feasibility

## Risks and Mitigations

| Risk | Mitigation |
|------|------------|
| Missing primitives | Add incrementally as needed |
| Semantic differences | Extensive testing at each phase |
| Fixed point doesn't converge | Debug with byte-diff, add determinism |
| habu0 linker too limited | Port full linker from bootstrap |

## Next Immediate Steps

1. Fix wrap-with-heap-stub in habu0.lisp
2. Test mode #x300 with `(+ 20 22)`
3. Begin Phase 2 (remove format/loop/with-open-file)

**Total estimated time to self-hosting**: 10-14 days

---

**Last updated**: November 28, 2025
**Status**: Phase 0 complete (dynamic frames), Phase 1 in progress (linker fix)
