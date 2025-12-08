# Labels Support Test Results for Habu Compiler

## Test Date: 2025-12-08

## Summary

The `labels` special form is **partially implemented** in the Habu compiler but has critical gaps in support:

### What Works ✓

1. **Compiler Frontend (compile-expr-full)**
   - Successfully parses and transforms `labels` expressions to IR
   - Correctly handles both non-recursive and recursive `labels` functions
   - The transformation is implemented in `/Users/joel/Work/habu/bootstrap/compiler.lisp` (lines 972-1071)
   - Transforms `labels` into let/setq/lambda/funcall with FNTAB (function table)

### What Doesn't Work ✗

1. **Code Generator (tac-codegen)**
   - **FAILS** when trying to generate machine code for `labels` expressions
   - Error: `"tac-codegen Pass 1: Unknown marker :MAKE-CLOSURE-MARKER from instruction TAC-MAKE-CLOSURE - needs implementation"`
   - The code generator doesn't know how to handle the closure creation needed for `labels`
   - This prevents JIT compilation and native code generation for any code using `labels`

2. **habu0 Interpreter (h0-eval)**
   - **NOT IMPLEMENTED** at all in the interpreter
   - The `h0-eval` function in `/Users/joel/Work/habu/habu0.lisp` (line 643) does not have a case for `labels`
   - Attempting to evaluate `labels` expressions results in: `"h0-eval: unknown function"`
   - Note: The habu0.lisp source code *uses* `labels` internally (e.g., lines 817, 841), but doesn't expose it as an evaluable form

3. **habu0 Binary Issues**
   - The current habu0 binary appears to have additional problems beyond just `labels`
   - Even simple expressions like `(+ 1 2)` fail with "unknown function" error
   - Only literal numbers work (e.g., `42` returns exit code 42)
   - This suggests the binary may be corrupted or incompletely built

## Detailed Test Cases

### Test 1: Simple Non-Recursive Labels
```lisp
(labels ((helper (x) (+ x 1)))
  (helper 5))
```
**Expected**: 6
**SBCL Compiler (compile-expr-full)**: ✓ Compiles to IR successfully
**SBCL JIT (jit-compile-expression)**: ✗ Fails at codegen with :MAKE-CLOSURE-MARKER error
**habu0 Interpreter**: ✗ "h0-eval: unknown function"

### Test 2: Recursive Labels (Factorial)
```lisp
(labels ((fact (n)
           (if (< n 2)
               1
               (* n (fact (- n 1))))))
  (fact 5))
```
**Expected**: 120
**SBCL Compiler (compile-expr-full)**: ✓ Compiles to IR successfully
**SBCL JIT (jit-compile-expression)**: ✗ Fails at codegen with :MAKE-CLOSURE-MARKER error
**habu0 Interpreter**: ✗ "h0-eval: unknown function"

### Test 3: Defun with Labels Helper
```lisp
(defun test-labels ()
  (labels ((helper (x) (+ x 1)))
    (helper 5)))
(test-labels)
```
**Expected**: 6
**Status**: Not tested due to issues with simpler cases

## Technical Details

### Compiler Architecture
The labels implementation follows this path:
1. `compile-expr-full` (line 1123) → detects `labels` special form
2. `compile-labels` (line 972) → transforms to IR using let/setq/funcall
3. Should go to `tac-codegen` for machine code generation ← **BREAKS HERE**

### The Missing Piece
The codegen phase doesn't implement the `:MAKE-CLOSURE-MARKER` handler needed for creating closures from `labels`-generated lambdas. The TAC (Three-Address Code) intermediate representation creates `TAC-MAKE-CLOSURE` instructions, but `tac-codegen` doesn't know how to convert these to machine code.

### IR Example
For `(labels ((helper (x) (+ x 1))) (helper 5))`, compile-expr-full produces:
```lisp
(LET-IR ((NIL-IR))
 (PROGN-IR
  ((SETQ-IR 0
    (LAMBDA-IR (#:FNTAB128 X)
     (LET-IR ((CAR-IR (VAR 0))) (ADD (VAR 1) (LIT 1)) 1 (2)) NIL NIL))
   (LET-IR ((CONS-IR (VAR 0) (NIL-IR)))
    (FUNCALL-IR (VAR 0) ((VAR 1) (LIT 5))) 1 (1))))
 1 (0))
```

## Recommendations

To fix `labels` support, you need to:

1. **Fix habu0 Binary First** - Rebuild the binary to verify basic functionality works
2. **Implement :MAKE-CLOSURE-MARKER in tac-codegen** - Add handler for closure creation in the code generator
3. **Add labels Support to h0-eval** - Implement labels as a special form in the interpreter
4. **Consider Alternative**: If closure codegen is too complex, implement labels using defun at the top level (though this changes semantics)

## Files Involved

- `/Users/joel/Work/habu/bootstrap/compiler.lisp` - Has compile-labels implementation
- `/Users/joel/Work/habu/bootstrap/codegen.lisp` or `/Users/joel/Work/habu/bootstrap/reg-alloc.lisp` - Need :MAKE-CLOSURE-MARKER handler
- `/Users/joel/Work/habu/habu0.lisp` - h0-eval needs labels case added
