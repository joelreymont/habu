# Runtime Table + JIT Integration Plan

## Goals
- Use real runtime addresses (tiny C runtime) in ARM64 codegen and JIT execution.
- Keep SBCL bring-up safe (no REPL crashes), opt-in JIT on ARM64.
- Prefer tiny C helper (`libhabu-jit.*`) when present, fallback to mmap path for dev.

## Current State
- SBCL smoke loads stub codegen only; env/helper provides runtime addrs for hexdump.
- `bin/print-runtime-addrs` emits HABU_* addrs; `*runtime-addrs*` defaults used in codegen entrypoints.
- Non-stub `compile-to-arm64` and `compile-program-with-functions` now default to `*runtime-addrs*`.
- JIT scaffold in `run-habu.lisp` opt-in and ARM64-gated; uses helper if available.

## Step-by-Step Implementation
1) **Cache runtime table in pure Lisp path**
   - Add `*runtime-addrs*` init in main Lisp runtime (non-SBCL) from helper/env before codegen/JIT.
   - Provide helper `(ensure-runtime-addrs)` callable by REPL, loader, JIT entry.

2) **Plumb into real codegen**
   - Ensure all codegen entrypoints (`compile-to-arm64(-with-runtime)`, `compile-program-with-functions`) read `*runtime-addrs*` when runtime arg is nil.
   - Thread runtime through higher-level compile pipeline (programs/functions) consistently.

3) **Lisp JIT entrypoint**
   - Add `(jit-eval expr)` that: ensure runtime table, compile via real codegen, execute via helper/mmap, return result.
   - ARM64-only guard; skip on other hosts.

4) **Tests**
   - Script to run `(jit-eval '(cons 1 2))`, `(jit-eval '(car (cons 1 2)))`, `(jit-eval '(cdr (cons 1 2)))` using helper-provided addrs; assert returned tagged fixnums.
   - Gate tests to ARM64 and helper availability; otherwise skip.

5) **CI/Automation**
   - Extend `run-habu-lisp-ci.sh` to optionally run JIT smoke when ARM64 + helper present; otherwise keep stub-only checks.

6) **Docs**
   - Document helper/env expectations and `jit-eval` usage; note tiny C runtime requirement and ARM64-only status.

## Constraints
- Tiny C runtime only; no C backend.
- Keep SBCL path non-crashing; JIT opt-in.
- Hex literals for runtime addrs.
