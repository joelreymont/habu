---
title: Add IR representation hints for JIT specialization
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-06T06:34:06.053605+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

From JIT design analysis: To enable specialized code generation, the existing IR needs optional type/representation hints WITHOUT changing the core IR structure.

## Goal

Extend the tagged IR with representation hints to enable specialized codegen while keeping the IR stable.

## Design (from HABU_ARM64_JIT_ARCH.md)

### Option 1: Inline Hints

Add :rep keyword to IR forms:

```lisp
;; Current
(call-fn 'add (list arg1-ir arg2-ir))

;; With hints
(call-fn :rep '(:fixnum :fixnum)
         'add
         (list arg1-ir arg2-ir))
```

### Option 2: Side Table (Recommended)

Keep IR unchanged, add annotation mapping:

```lisp
;; Global or per-function table
*rep-hints* = 
'((add . ((0 . :fixnum) (1 . :fixnum)))
  (mul . ((0 . :fixnum) (1 . :fixnum))))

;; Helper in codegen
(defun arg-rep-hint (call-ir arg-index)
  (lookup-in *rep-hints* call-ir arg-index))
```

### Representation Classes

- `:fixnum` - unboxed 64-bit integer
- `:double` - unboxed IEEE 754 double  
- `:tagged` - boxed/generic value
- `:pair` - known cons cell
- `:vector` - known vector

## Implementation Strategy

1. **Phase 1: Infrastructure**
   - Define rep-hint data structure
   - Add arg-rep-hint lookup function
   - No codegen changes yet

2. **Phase 2: Hint Propagation**
   - Type inference pass populates rep-hints
   - compile-specialized uses hints

3. **Phase 3: Specialized Codegen**
   - codegen-expr checks hints for optimizations
   - Skip tag checks when rep known
   - Use unboxed arithmetic for :fixnum

## Integration with arm64/codegen.lisp

In call-fn and call-closure cases:

```lisp
(defun codegen-call-fn (fn-name args ...)
  (let ((arg-reps (map 'list #'arg-rep-hint args)))
    (if (all-fixnum? arg-reps)
        (codegen-fixnum-call fn-name args)  ; specialized
        (codegen-generic-call fn-name args)))) ; generic
```

## Key Principles (from design doc)

- **DON'T** change codegen-expr's core pattern matching
- **DO** add small helpers that use hints
- **DON'T** invent new IR
- **DO** keep side table approach simple

## Tasks

1. Define rep-hint data structure
2. Add arg-rep-hint helper function
3. Wire into compile-specialized workflow
4. Update codegen-expr to check hints
5. Implement specialized codegen for fixnum arithmetic
6. Test: verify specialized code is generated when hints present

## Dependencies

- Type inference (habu-8hdb) to populate hints
- Function versioning (habu-07lo) for multi-version compilation

## Benefits

- Enables specialized code without IR redesign
- Minimal risk - side table approach is non-invasive
- Easy to evolve - can add more rep classes incrementally

## References

- /Users/joel/Downloads/habu-jit-gc-package/Habu_Codegen_and_JIT.md
- Section 2.1: "Keep existing IR, add rep hints"
