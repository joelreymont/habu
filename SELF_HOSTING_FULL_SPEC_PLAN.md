# Self-Hosting Full-Spec Lisp — Small-Step Checklist

## Stage #x00: Hygiene & Ground Rules
- [ ] #x00 Keep `SESSION_CONTEXT.md` updated every working block with decisions, blockers, and next steps.
- [x] #x01 Record agent constraints (tiny C runtime only, hex literals, commit often) and enforce in all new code/doc.
- [ ] #x02 Remove/archivize C backend artifacts so only the tiny C runtime remains in the active tree.
- [ ] #x03 Track all work in git with small, focused commits (plan, cleanup, bring-up fixes).

## Stage #x01: Pure-Lisp Bring-Up (SBCL Host)
- [x] #x10 Provide SBCL shim package with predicates/string helpers (`sbcl-habu-shim.lisp`).
- [x] #x11 Add SBCL-only stub codegen module exporting `compile-to-arm64` for bring-up.
- [x] #x12 Make `run-habu.lisp` load only Lisp path (no C backend), then run smoke compile.
- [x] #x13 Add SBCL harness to print hexdump of stubbed bytes and exit zero on success.
  - [x] #x13a Make stub codegen emit deterministic non-empty hex bytes (prologue + ret placeholder).
  - [x] #x13b Add simple hexdump formatter in `run-habu.lisp` showing length and hex pairs.
  - [x] #x13c Ensure smoke path exits zero after hexdump even with stub runtime.
- [x] #x14 Add CI-style script to run bring-up (`run-habu-lisp.sh`) and capture log.

## Stage #x02: C-Backend Cleanup (Tiny C Runtime Only)
- [x] #x20 Move legacy C codegen scripts (`c-codegen.lisp`, `ir-to-c.lisp`, `compile-habu.sh`) into `archive/legacy-c-backend/`.
- [x] #x21 Remove tracked C backend binaries/artifacts (e.g., `habu-extended`, `habu-enhanced`, `habu-prog`, `habu-rec`, `complete-macho-gen`) from the active tree.
- [x] #x22 Keep only tiny C runtime + JIT helper; document retained C surface in `SESSION_CONTEXT.md`.
- [x] #x23 Add README in `archive/legacy-c-backend/` explaining why archived and current pure-Lisp path.

## Stage #x03: Runtime Foundation (Tiny C)
- [ ] #x30 Audit runtime API surface (cons, car/cdr, symbols, strings, vectors, I/O, GC entry points); list gaps.
- [ ] #x31 Add runtime table builder in Lisp that captures all function pointers (cons/car/cdr/alloc/string/vector/IO/conditions).
- [ ] #x32 Thread runtime table through all codegen entry points (expr, functions, programs).
- [ ] #x33 Add error/condition stubs in runtime for signal/error/cerror/warn.
- [ ] #x34 Add allocation fast-path helpers (cons, string, vector) with slow-path hooks to GC.
- [ ] #x35 Document calling convention + runtime table shape in code comments and a one-page markdown.

## Stage #x04: Reader & Printer (Full Spec)
- [ ] #x40 Implement reader macros: `'`, `` ` ``, `,`, `,@`, `#()`, `#.` , `#'`, `#\\char`, `#| ... |#`, dispatch table hooks.
- [ ] #x41 Support radix integers and bignum parsing; honor `*read-base*`.
- [ ] #x42 Implement character literals and named characters.
- [ ] #x43 Add pretty printer hooks; minimal readable printer that handles cycles/shared structure detection.
- [ ] #x44 Add tests for reader/printer parity (round-trip + golden cases).

## Stage #x05: Evaluator/Compiler Core (Lisp)
- [ ] #x50 Special forms coverage: quote, if, progn, let/let*/letrec, setq, function, block/return-from, tagbody/go, catch/throw, unwind-protect, progv.
- [ ] #x51 Lexical + dynamic environment model; environment objects with lookup and updates; global environment persistence across load.
- [ ] #x52 Closure capture analysis: free-var detection, environment layout, boxed capture for mutation.
- [ ] #x53 Macro system: defmacro, macrolet, symbol-macrolet, macroexpand-1/macroexpand; compiler-macro hooks.
- [ ] #x54 Multiple values: `values`, `values-list`, `multiple-value-bind/call`, propagation through primitives and calls.
- [ ] #x55 Type declarations parsing (`declare`, `the`, `optimize`, `safety/speed/debug` hints) — initially ignored but parsed.
- [ ] #x56 Error system hooks: `signal`, `error`, `cerror`, `warn`, `handler-bind/case`, `restart-case/bind`, `ignore-errors`.

## Stage #x06: IR + Transforms
- [ ] #x60 Define canonical IR (already present) — ensure tags for literals, vars, calls, closures, control.
- [ ] #x61 Add closure conversion pass that rewrites lambdas with explicit env loads.
- [ ] #x62 Add tail-position analysis for TCO flags.
- [ ] #x63 Add basic optimizations: constant folding, dead code elimination for progn/if, simple inlining budget.
- [ ] #x64 Add spill analysis framework for registers (ARM64 first).

## Stage #x07: ARM64 Codegen Completion
- [ ] #x70 Ensure all primitives compile: arithmetic, comparisons, logic, cons/car/cdr (done), strings, vectors, symbols, I/O calls.
- [ ] #x71 Implement block/return-from, tagbody/go lowering with labels/branches.
- [ ] #x72 Implement catch/throw and unwind-protect codegen (stack unwinding + handler table).
- [ ] #x73 Implement multiple values ABI (return registers + stack slots).
- [ ] #x74 Implement closures: allocate env, store captured vars, load in callees.
- [ ] #x75 Implement function calls with proper call frame (save/restore LR/FP, spill temps), varargs policy if needed.
- [ ] #x76 Add GC-safe points / frame maps or conservative policy for JIT frames.
- [ ] #x77 Add instruction encoders for any missing ops; ensure all literals use hex representation.
- [ ] #x78 Add hexdump/disasm helper to verify emitted bytes.

## Stage #x08: JIT Harness & Tests (ARM64)
- [ ] #x80 Wire runtime addresses from tiny C runtime into Lisp compiler before codegen.
- [ ] #x81 Add JIT executor in Lisp that mmaps RWX (via tiny C helper) and runs generated bytes.
  - [x] #x81a SBCL-only mmap/RWX scaffold (optional icache flush, opt-in flag).
  - [ ] #x81b macOS entitlement/`MAP_JIT` path via tiny C helper (`habu-jit.c`) or signed binary.
  - [ ] #x81c Switch default to tiny C helper for portability; keep SBCL path as dev-only.
- [ ] #x82 Add tests for: arithmetic, control, cons/car/cdr, strings, vectors, closures, recursion, multiple values, error paths.
- [ ] #x83 Add load-file -> compile -> JIT -> execute pipeline tests.
- [ ] #x84 Add benchmarks (small) to sanity-check performance.

## Stage #x09: REPL & Loader (Pure Lisp)
- [ ] #x90 Harden `(load ...)` to support packages, multiple forms, and reader conditionals.
- [ ] #x91 Pure-Lisp REPL with history/readline hook via runtime; error trapping + restarts.
- [ ] #x92 Add `:reload`, `:disasm`, `:jit` commands to inspect generated code.
- [ ] #x93 Ensure REPL uses pure-Lisp compiler path (no C backend), threads runtime table.

## Stage #x0A: Self-Hosting Milestones
- [ ] #xA0 Compile simple programs end-to-end with real runtime addrs (cons/string/vector/IO).
- [ ] #xA1 Run recursive functions (factorial/fib) through JIT and compare with interpreter.
- [ ] #xA2 Compile compiler with itself (stage1), compile again (stage2), compare byte-for-byte or hash (fixed point).
- [ ] #xA3 Package stage1/2 artifacts; document bootstrap procedure.
- [ ] #xA4 Add automation script to reproduce bootstrap from clean checkout using SBCL host + tiny runtime.

## Stage #x0B: x86_64 Target
- [ ] #xB0 Port ARM64 IR lowering to x86_64 encoders; ensure feature parity.
- [ ] #xB1 Implement runtime address plumbing for x86_64; reuse runtime table builder.
- [ ] #xB2 Add JIT executor for x86_64 (tiny C helper or OS APIs).
- [ ] #xB3 Add x86_64 test suite parallel to ARM64 tests.

## Stage #x0C: Compliance, Perf, Tooling
- [ ] #xC0 Fill numeric tower gaps: bignum, ratio, float, complex arithmetic + predicates.
- [ ] #xC1 Implement packages fully (defpackage, in-package, export/import/use, keyword package semantics).
- [ ] #xC2 Implement hash tables (eq/eql/equal/equalp) and sequences API (adjustable arrays, fill-pointer, bit-vectors).
- [ ] #xC3 Add condition/restart hierarchy tests; ensure error paths integrated with runtime.
- [ ] #xC4 Build compliance checklist against CL spec and track progress.
- [ ] #xC5 Add profiling/tracing hooks; optional disassembler integration.
- [ ] #xC6 Add developer docs for calling conventions, runtime table, reader/printer, and bootstrap steps.

## Stage #x0D: Release Hygiene
- [ ] #xD0 Keep repo free of generated C/backend artifacts; verify `.gitignore` blocks new ones.
- [ ] #xD1 Add `make`/scripts for pure-Lisp workflows only (bring-up, compile, JIT tests).
- [ ] #xD2 Tag milestones (bring-up, JIT parity, self-hosting, full compliance) and summarize in `SESSION_CONTEXT.md`.
