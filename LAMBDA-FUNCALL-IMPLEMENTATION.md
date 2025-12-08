# LAMBDA and FUNCALL Implementation for h0-compile

## Summary

This implementation adds minimal viable LAMBDA and FUNCALL support to the h0-compile compiler in habu0.lisp. The implementation includes:

1. **IR Representation** - New IR tags for lambda and funcall
2. **Free Variable Detection** - Complete analysis of closure captures
3. **Compilation Infrastructure** - IR generation for closures
4. **Codegen Stubs** - Placeholder implementations

## Files Modified

- `habu0.lisp` - Main implementation file
- `lambda-funcall-patch.lisp` - Reference implementation and documentation
- `test-lambda-compile.lisp` - Test cases

## Implementation Details

### 1. IR Tags (Lines 1566-1567)

```lisp
(defun ir-tag-lambda () #x22)     ; lambda (closure creation)
(defun ir-tag-funcall () #x23)    ; funcall (closure invocation)
```

### 2. Free Variable Analysis (Lines 1570-1700)

Complete free variable detection infrastructure with the following components:

- `h0-find-free-vars` - Main entry point for free variable analysis
- `h0-collect-free` - Recursively collect free variables from expressions
- `h0-in-env` - Check if a symbol is in the compilation environment
- `h0-member-sym` - Check symbol membership using string comparison
- `h0-get-var-offset` - Get stack offset for a variable
- `h0-get-free-offsets` - Get offsets for all free variables
- `h0-make-param-env` - Build environment for lambda body
- Helper functions for list operations and binding analysis

#### How Free Variable Detection Works

```lisp
;; Example: (lambda (x) (+ x y))
;; where y is in outer environment
;;
;; 1. params = (x)
;; 2. body = (+ x y)
;; 3. env = ((\"Y\" . nil) ...)  ; y is at some offset
;; 4. free-vars = (Y)  ; x is bound, y is free
;; 5. free-offsets = (2)  ; y is at offset 2 in env
```

The free variable analyzer:
- Walks the lambda body AST
- Tracks bound variables (parameters + let bindings)
- Identifies variables in environment but not bound
- Computes stack offsets for captured variables

### 3. Lambda Compilation (Lines 1868-1876)

```lisp
((sym= op "LAMBDA")
 (let* ((params (cadr expr))
        (body (caddr expr))
        (free-vars (h0-find-free-vars body params env))
        (free-offsets (h0-get-free-offsets free-vars env))
        (param-env (h0-make-param-env params free-vars))
        (body-ir (h0-compile body param-env fenv)))
   (list (ir-tag-lambda) params body-ir free-vars free-offsets)))
```

#### Lambda IR Format

```
(ir-tag-lambda params body-ir free-vars free-offsets)

Where:
- params: List of parameter symbols
- body-ir: Compiled IR for the lambda body
- free-vars: List of free variable symbols
- free-offsets: List of stack offsets for free vars in current env
```

#### Environment Layout for Lambda Body

Parameters come first, then free variables:
```
Offset 0: param1
Offset 1: param2
...
Offset n: free-var1
Offset n+1: free-var2
...
```

### 4. Funcall Compilation (Lines 1877-1882)

```lisp
((sym= op "FUNCALL")
 (let* ((fn-ir (h0-compile (cadr expr) env fenv))
        (args (cddr expr))
        (args-ir (h0-compile-args args env fenv)))
   (list (ir-tag-funcall) fn-ir args-ir)))
```

#### Funcall IR Format

```
(ir-tag-funcall fn-ir args-ir-list)

Where:
- fn-ir: IR for the function expression (evaluates to closure)
- args-ir-list: List of compiled argument IRs
```

### 5. Codegen Stubs (Lines 2616-2625)

Both lambda and funcall have stub implementations that error at codegen time:

```lisp
((h0-has-tag-n ir (ir-tag-lambda))
 (fatal-error "h0-codegen: LAMBDA not yet implemented"))

((h0-has-tag-n ir (ir-tag-funcall))
 (fatal-error "h0-codegen: FUNCALL not yet implemented"))
```

## What Works

✅ **Compilation to IR**: Lambdas compile to IR with proper free variable analysis
✅ **Free Variable Detection**: Correctly identifies and tracks captured variables
✅ **Nested Lambdas**: Handles nested closures with multiple capture levels
✅ **Environment Management**: Proper offset calculation for parameters and captures
✅ **Funcall IR**: Function calls compile to IR with function and argument expressions

## What Doesn't Work Yet

❌ **Code Generation**: No ARM64 machine code generation
❌ **Lambda Lifting**: Lambdas aren't extracted to top-level functions
❌ **Closure Allocation**: No heap allocation for closure objects
❌ **Calling Convention**: No runtime support for calling closures
❌ **Execution**: Programs with lambda/funcall will error at codegen

## Next Steps for Full Implementation

### 1. Lambda Lifting

Extract lambda-ir nodes to top-level functions:

```lisp
;; Before lifting:
(let ((y 10))
  (lambda (x) (+ x y)))

;; After lifting:
;; Top-level: (defun LAMBDA-1 (y x) (+ x y))
;; In code: (closure LAMBDA-1 (y))
```

### 2. Closure Representation

Define closure heap layout:
```
[tag:3][num-captures:8][fn-ptr:8][capture1:8][capture2:8]...
```

Tag 5 for closures, differentiated from other heap objects.

### 3. Closure Creation Codegen

```
1. Allocate space: 16 + (num-captures * 8) bytes
2. Store num-captures at [heap+0]
3. Store function pointer at [heap+8]
4. Store captured values at [heap+16+]
5. Return tagged pointer (heap | 5)
```

### 4. Funcall Codegen

```
1. Evaluate closure expression -> x0 (tagged pointer)
2. Untag: x0 = x0 - 5
3. Load num-captures from [x0]
4. Load fn-ptr from [x0+8]
5. Load captures from [x0+16+]
6. Evaluate arguments
7. Build frame: [captures...][args...]
8. BLR to fn-ptr
9. Restore frame
```

### 5. Calling Convention

Design register and stack usage:
- x0-x7: First 8 arguments
- Stack: Additional args + captured values
- x20: Frame pointer
- x30: Return address

## Test Cases

See `test-lambda-compile.lisp` for test cases:

1. Simple lambda (no captures)
2. Lambda with free variables
3. Funcall with literal lambda
4. Funcall with variable
5. Nested lambdas

## Reference Implementation

The bootstrap compiler (bootstrap/compiler-sbcl.lisp) provides reference implementation:

- Lines 1182-1342: find-free-vars implementation
- Lines 1900-1914: Lambda compilation
- Lines 2153-2157: Funcall compilation
- Lines 3205-3219: Lambda IR evaluation
- Lines 3159-3189: Funcall IR evaluation
- Lines 3910-3927: Lambda lifting

## Integration Notes

This implementation:
- Uses string-based symbol comparison (sym=) throughout
- Follows habu0.lisp conventions for IR format and compilation
- Is self-hosting compatible (uses only available primitives)
- Maintains compatibility with existing h0-compile infrastructure

## Backup

Original file backed up to: `habu0.lisp.backup`

## Conclusion

This implementation provides the foundation for LAMBDA and FUNCALL support in h0-compile:

1. ✅ Complete IR representation
2. ✅ Full free variable analysis
3. ✅ Proper environment management
4. ✅ Compilation infrastructure
5. ⏳ Codegen stubs (for future implementation)

The IR compilation works correctly; only the ARM64 code generation remains to be implemented. This is appropriate for an MVP, as lambda lifting and closure code generation are substantial undertakings that require careful design of the runtime calling convention.
