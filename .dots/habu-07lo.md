---
title: Add JIT function versioning infrastructure
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-06T06:32:34.779463+02:00"
closed-at: "2025-12-25 07:22:05"
close-reason: "Obsolete: Zig rewrite"
---

## Context

From JIT/GC design analysis: Habu already has the foundation for JIT compilation (arm64/asm.lisp, arm64/codegen.lisp), but lacks runtime multi-versioning for specialized code.

## Goal

Implement function versioning to allow multiple specialized implementations of the same function, selected at runtime based on argument types.

## Design (from HABU_ARM64_JIT_ARCH.md)

### Data Structures

```lisp
(defstruct function-version
  (rep-key nil)   ; e.g. '(:fixnum :fixnum) - argument type signature
  (code nil)      ; code pointer / executable memory
  (hotness 0))    ; call count for this version

(defstruct function-object
  name
  env                   ; closure environment pointer
  generic-code          ; baseline code pointer (fallback)
  (versions '()))       ; list of FUNCTION-VERSION structs
```

### Implementation Strategy

1. **Phase 1: Lisp-level dispatcher**
   - Implement dispatcher function that:
     - Computes rep-key from runtime argument types
     - Looks up matching function-version
     - Calls specialized code if found, else generic
   - No changes to arm64/asm.lisp or codegen initially
   - All policy logic in Lisp (easy to evolve)

2. **Phase 2: Specializing recompile path**
   - Store IR/AST for each function
   - API: `(compile-specialized fn-name rep-key)`
   - Annotate IR with type hints from rep-key
   - Reuse existing arm64/codegen.lisp with hints

3. **Phase 3: ARM64 entry stubs** (future)
   - Generate small ARM64 stub that checks types
   - Direct jump to specialized code on match
   - Better performance than Lisp dispatcher

## Key Insights

- **DO NOT** create new IR - extend existing tagged IR with optional hints
- **DO NOT** create new backend - reuse arm64/codegen.lisp
- Function versioning lives in Lisp heap (not C runtime)
- This is "compile multiple versions" not "compile during execution"

## Tasks

1. Define function-version and function-object structs
2. Implement Lisp-level dispatcher
3. Store IR for functions in global table
4. Implement compile-specialized function
5. Wire into compiler to produce function-objects instead of raw pointers
6. Add profiling/hotness tracking

## Integration

- Modify native-compiler-main.lisp / compiler-driver.lisp
- Keep closure representation unchanged in C runtime
- All versioning metadata lives in Habu Lisp heap

## Dependencies

- Type inference (habu-8hdb) for choosing rep-keys
- IR preservation (need to keep original IR/AST)

## References

- /Users/joel/Downloads/habu-jit-gc-package/Habu_Codegen_and_JIT.md
- Section 2: "How to layer a JIT on top of this design"
