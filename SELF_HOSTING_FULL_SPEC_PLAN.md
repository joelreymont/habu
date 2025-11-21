# Self-Hosting Full-Spec Lisp — Small-Step Plan

## Stage #x00: Hygiene & Ground Rules
- [x] #x00 Note constraints: tiny C runtime only; hex literals; commit frequently; keep SESSION_CONTEXT updated.
- [ ] #x01 Keep SESSION_CONTEXT.md current each work block (decisions, blockers, next steps).
- [ ] #x02 Run `run-habu-lisp.sh` smoke after each SBCL-facing change; keep `run-habu-lisp-ci.sh` green.
- [ ] #x03 Avoid C backend artifacts; keep only tiny runtime + JIT helper; archive new strays immediately.

## Stage #x01: Pure-Lisp Bring-Up (SBCL Host)
- [x] #x10 SBCL shim for predicates/strings (`sbcl-habu-shim.lisp`).
- [x] #x11 SBCL stub codegen exporting `compile-to-arm64`.
- [x] #x12 Smoke compile in `run-habu.lisp` (no C backend) with hexdump.
- [x] #x13 CI wrapper `run-habu-lisp-ci.sh` checking smoke output.
- [x] #x14 Optional SBCL JIT scaffold (opt-in) + tiny C helper autodetect (`libhabu-jit.*`).
- [x] #x15 Add SBCL hexdump content assertion (length/opcodes) to CI wrapper.

## Stage #x02: Runtime Wiring (Tiny C Only)
- [ ] #x20 Expose runtime addresses from tiny C runtime (`habu_cons`/`car`/`cdr` etc.) via one table builder callable from Lisp.
- [ ] #x21 Thread runtime table through real ARM64 codegen entry points (not stubs).
- [ ] #x22 Validate cons/car/cdr JIT using real addresses via Lisp->JIT path (no manual C harness).
- [ ] #x23 Extend table to strings/vectors/symbols/IO/errors; document shape.

## Stage #x03: Reader/Printer Completion
- [ ] #x30 Implement remaining reader macros (#', `, ,@, #(), #., char literals, #| |#, dispatch table).
- [ ] #x31 Implement printer with readable/unreadable modes, cycle/share detection.
- [ ] #x32 Add reader/printer round-trip tests.

## Stage #x04: Evaluator/Compiler Core
- [ ] #x40 Special forms coverage: block/return-from, tagbody/go, catch/throw, unwind-protect, progv.
- [ ] #x41 Defmacro + macroexpansion hooks; macrolet/symbol-macrolet.
- [ ] #x42 Multiple values plumbing (values, mv-bind/call).
- [ ] #x43 Env model: lexical+dynamic; global env persists across load; package-aware lookup.
- [ ] #x44 Declarations parsed (optimize/type/the) initially no-op.
- [ ] #x45 Error/condition stubs: signal/error/cerror/warn, handler-bind/case, restart-case/bind.

## Stage #x05: IR + Transforms
- [ ] #x50 Closure conversion/free-var capture.
- [ ] #x51 Tail-position analysis (TCO flags).
- [ ] #x52 Basic opts: const-fold, dead-progn/if trim, tiny inlining budget.
- [ ] #x53 Spill analysis scaffold (ARM64 first).

## Stage #x06: ARM64 Codegen Completion
- [ ] #x60 Runtime call table coverage (strings/vectors/symbols/IO/errors/GC) beyond cons/car/cdr.
- [ ] #x61 Block/return, tagbody/go lowering with branches/labels.
- [ ] #x62 Catch/throw + unwind-protect codegen (handler/unwind tables).
- [ ] #x63 Multiple values ABI (regs/stack).
- [ ] #x64 Closures: allocate env, store captures, load in callees.
- [ ] #x65 Call frames: save/restore LR/FP, spills, varargs policy.
- [ ] #x66 GC-safe points or conservative policy for JIT frames.
- [ ] #x67 Hex-only literals; add hexdump/disasm helper for verification.

## Stage #x07: JIT Harness & Tests (ARM64)
- [ ] #x70 Prefer tiny C helper (`libhabu-jit.*`) for exec; sign binary with `MAP_JIT` on macOS as needed.
- [ ] #x71 Add JIT tests for arithmetic/control/cons/strings/vectors/closures/recursion/mv/errors using real runtime table.
- [ ] #x72 Add load-file -> compile -> JIT -> execute integration test.
- [ ] #x73 Add lightweight benchmarks (fixnums, cons, calls).

## Stage #x08: REPL & Loader (Pure Lisp)
- [ ] #x80 Harden `(load ...)` with packages/readtable conditionals; env threading.
- [ ] #x81 Pure-Lisp REPL with history via runtime lineedit; error trapping + restarts; :reload/:jit/:disasm commands.
- [ ] #x82 Ensure REPL uses pure-Lisp compiler path, threads runtime table.

## Stage #x09: Self-Hosting Milestones
- [ ] #x90 Compile simple programs with real runtime addrs end-to-end (JIT run).
- [ ] #x91 Run recursion tests (factorial/fib) through JIT vs interpreter.
- [ ] #x92 Compile compiler with itself (stage1), then stage2; compare outputs (fixed point).
- [ ] #x93 Automate bootstrap script (SBCL host + tiny runtime -> stage2).

## Stage #x0A: x86_64 Target
- [ ] #xA0 Port ARM64 lowering to x86_64 encoders; runtime plumbing.
- [ ] #xA1 JIT executor for x86_64; parity tests mirroring ARM64.

## Stage #x0B: Compliance & Data Structures
- [ ] #xB0 Numeric tower: bignum/ratio/float/complex arithmetic + predicates.
- [ ] #xB1 Packages fully implemented (defpackage/in-package/export/import/use/keywords).
- [ ] #xB2 Hash tables (eq/eql/equal/equalp) and sequence APIs (adjustable arrays, fill-pointer, bit-vectors).
- [ ] #xB3 Conditions/restarts tests; integrate runtime error paths.

## Stage #x0C: Tooling & Docs
- [ ] #xC0 Update docs: calling conventions, runtime table, reader/printer, bootstrap steps.
- [ ] #xC1 Compliance checklist vs CL spec; track gaps.
- [ ] #xC2 Profiling/tracing hooks; optional disassembler/hexdump verifier.

## Stage #x0D: Release Hygiene
- [ ] #xD0 Keep repo free of generated C/backend artifacts; `.gitignore` updated as needed.
- [ ] #xD1 Scripts for pure-Lisp workflows (bring-up, compile, JIT tests) only.
- [ ] #xD2 Tag milestones (bring-up, JIT parity, self-hosting, full compliance) and record in SESSION_CONTEXT.md.
