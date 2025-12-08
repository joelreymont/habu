# LABELS and FLET Implementation in h0-compile

## Summary

Implemented LABELS and FLET support in h0-compile (habu0.lisp) for the Habu Lisp compiler's self-hosting path.

## Changes Made

### 1. IR Tags Added (lines 1566-1568)
- `ir-tag-lambda` (#x22) - lambda (closure creation) - already existed
- `ir-tag-funcall` (#x23) - funcall (closure invocation) - already existed
- `ir-tag-setq` (#x24) - setq (variable assignment) - **NEW**

### 2. Helper Functions for Transformation (lines 1531-1618)

Added helper functions to transform LABELS/FLET into simpler constructs:

- `h0-member-sym` - Check if symbol is in list (string comparison)
- `h0-rewrite-calls` - Rewrite function calls to use FUNCALL
- `h0-rewrite-calls-list` - Rewrite list of expressions
- `h0-rewrite-let-bindings` - Rewrite let bindings
- `h0-build-cons-chain` - Build cons chain for list construction
- `h0-build-accessor` - Build car/cdr accessor chain
- `h0-build-cdr-chain` - Build cdr chain
- `h0-build-fntab-unpack` - Build FNTAB unpack bindings for LABELS

### 3. Compilation Support in h0-compile (lines 2004-2018)

Added three new special form handlers:

**SETQ** (lines 2004-2012):
- Looks up variable in environment
- Compiles to IR with offset and value

**LABELS** (line 2014-2015):
- Delegates to `h0-compile-labels`

**FLET** (line 2017-2018):
- Delegates to `h0-compile-flet`

### 4. Main Transformation Functions (lines 1786-1901)

**h0-compile-labels** (lines 1786-1822):
Transforms LABELS using the function table (FNTAB) approach:

```lisp
(labels ((f1 (a) body1) (f2 (b) body2)) main-body)
=>
(let ((f1 nil) (f2 nil))
  (setq f1 (lambda (FNTAB a) (let ((f1 (car FNTAB)) (f2 (cadr FNTAB))) body1)))
  (setq f2 (lambda (FNTAB b) (let ((f1 (car FNTAB)) (f2 (cadr FNTAB))) body2)))
  (let ((FNTAB (cons f1 (cons f2 nil))))
    main-body-with-rewritten-calls))
```

**h0-compile-flet** (lines 1824-1841):
Transforms FLET into simple LET with lambdas:

```lisp
(flet ((f1 (a) body1) (f2 (b) body2)) main-body)
=>
(let ((f1 (lambda (a) body1))
      (f2 (lambda (b) body2)))
  main-body-with-rewritten-calls)
```

**Supporting functions**:
- `h0-extract-fn-names` - Extract function names from bindings
- `h0-make-nil-bindings` - Make nil bindings for LABELS initialization
- `h0-make-labels-setqs` - Generate SETQ forms for LABELS functions
- `h0-make-flet-bindings` - Generate LET bindings for FLET
- `h0-append-lists` - Append two lists

## Implementation Approach

### LABELS (Recursive Functions)
Uses the function table (FNTAB) pattern to enable mutual recursion:
1. Initialize all functions to nil in outer LET
2. SETQ each function to a lambda that takes FNTAB as first param
3. Inside each lambda, unpack FNTAB to bind function names
4. Create FNTAB as cons chain of all functions
5. Rewrite calls in main body to pass FNTAB: `(f args) -> (funcall f args)`
6. Rewrite calls in function bodies to pass FNTAB: `(f args) -> (funcall f FNTAB args)`

### FLET (Non-Recursive Functions)
Simpler transformation:
1. Transform to LET with lambda bindings
2. Rewrite calls to use FUNCALL: `(f args) -> (funcall f args)`
3. Functions cannot call themselves or each other recursively

## Testing

Created test file: `/Users/joel/Work/habu/test-labels-flet.lisp` with examples:
- Simple FLET with non-recursive function
- LABELS with recursive factorial
- LABELS with mutually recursive even-p/odd-p
- FLET with multiple functions
- LABELS with closure over outer variable

## Notes

- LAMBDA and FUNCALL were already implemented in h0-compile
- SETQ was added to support the LABELS transformation pattern
- The implementation follows the same pattern as bootstrap/compiler-sbcl.lisp
- Function calls are rewritten to use FUNCALL for indirection
- LABELS uses FNTAB parameter passing for proper recursive support
- Both forms integrate seamlessly with existing LET, LAMBDA, and FUNCALL support
