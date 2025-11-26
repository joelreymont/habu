# Self-Hosting Full-Spec Lisp TODO

- **Runtime (Tiny C Only)**
  - Verify runtime API completeness (alloc, GC, strings, vectors, symbols, I/O, errors, FFI hooks if any).
  - Add missing primitives for full spec: chars/char ops, bignums (or tagged fallback), floats, complex, ratios, byte-vectors.
  - Harden GC: precise tagging, root set, generational tuning, stack maps for JIT, finalizers/weak refs policy.

- **Reader/Printer**
  - Implement full reader macros: #' , `` ` `` , , , ,@ , #() vectors, #. eval, character literals, radix integers, #| ... |# comments, dispatch macros table.
  - Complete printer: readable/unreadable modes, gensym printing, circular/list sharing detection, pretty printer hooks.

- **Core Evaluator/Compiler (Lisp)**
  - Special forms: quote, if, progn, lambda, let/let*/letrec, labels/flet, setq, function, block/return-from, tagbody/go, catch/throw, unwind-protect, progv.
  - Macros: defmacro, macroexpansion hooks, macrolet, symbol-macrolet, compiler-macro support.
  - Environments: lexical + dynamic; global env for toplevel defs; package-aware symbol resolution.
  - Multiple values: propagate through apply/call, conditionals, values, mv-bind, mv-let, mv-call.
  - Type system: predicates, declarations parsing (optimize, safety, type, speed, debug), the, check-type hooks (no-op ok initially).
  - Error system hooks: signal, handler-bind/case, restart-case/bind, condition hierarchy stubs.

- **ARM64 Codegen (Primary Target)**
  - Thread runtime addresses everywhere (cons/car/cdr done); add full runtime call table (strings, symbols, vectors, I/O, errors, GC).
  - Function calls: proper calling convention, stack frames, spill/restore, varargs policy, tail-call where safe.
  - Closures: environment capture layout, allocation, load captured vars.
  - Heap allocation: fast paths for cons/strings/vectors, slow path to runtime when needed.
  - Arithmetic: fixnums complete; add bignum/float dispatch stubs; tagged numeric tower promotions.
  - Control flow: cond/when/unless done; add block/return, tagbody/go, catch/throw, unwind-protect lowering.
  - Multiple values: return-value registers/stack slots, caller handling.
  - GC maps: ensure frame maps or conservative policy for JIT frames.
  - Quasiquote expansion before codegen; macroexpansion pipeline.

- **x86_64 Codegen (Secondary Target)**
  - Parity with ARM64 feature set; reuse IR; target register conventions and call ABI; runtime address plumbing.

- **Packages & Symbols**
  - Implement defpackage, in-package, export/import/use/new-package/find-symbol/make-symbol, keyword package, gensym, readtable/package integration.

- **Numbers & Math**
  - Bignums: add tagged representation or boxed heap; implement add/sub/mul/div/mod/shift/bit ops with promotion.
  - Ratios, floats, complexes: parsing, arithmetic, comparisons, printing, type predicates.

- **Sequences & Data Structures**
  - Lists, vectors, strings done; add adjustable arrays, fill-pointer, bit-vectors, hash tables (EQ/EQL/EQUAL/EQUALP), alists/plists helpers.

- **Conditions & Restarts**
  - Condition types hierarchy, signal/error/cerror/warn, handlers, restarts, ignore-errors, handler-case/bind, restart-case/bind.

- **Compiler Pipeline**
  - IR pass: closure conversion, free-var analysis, lambda lifting strategy.
  - Optimizations (later): constant folding, dead code, inlining budget, TCO opportunities.
  - File loader: reliable load multi-form; package-aware loading; fasl or cached code optional later.
  - Self-hosting build: compile compiler with itself -> fixed point check.

- **REPL & Tooling**
  - Pure-Lisp REPL (no C backend) with readline hook (from runtime), printer, error trapping, restarts (recoverable), inspect stack, :reload/:quit commands.
  - Tracing/profiling hooks; disassembler or hexdump for generated code (optional).

- **Testing & Compliance**
  - Expand test suites: reader, printer, packages, macros, closures, multiple values, conditions/restarts, numeric tower, arrays/hash-tables.
  - Golden tests for codegen (ARM64 first, x86_64 next); JIT exec tests for all runtime calls with real addresses.
  - Compliance checklist vs Common Lisp (core). Mark gaps and non-goals.

- **Build & Artifacts**
  - Keep tiny C runtime only; no C backend outputs committed.
  - Scripts to run pure-Lisp REPL, compile-to-bytes, JIT executor; no make habu via C backend.

- **Bootstrap Milestones**
  1) Load compiler via load in Lisp REPL; run simple compile+exec with real runtime addrs.
  2) Run recursive tests (factorial/fib) through ARM64 JIT.
  3) Compile compiler with itself (stage1), then again (stage2); verify stage1 == stage2 (fixed point).
  4) Parity on x86_64.
  5) Fill remaining CL spec gaps (packages, conditions, macros edge cases, numeric tower).

- **Documentation & Tracking**
  - Keep SESSION_CONTEXT.md updated with progress, issues, and next steps.
  - Document calling conventions, runtime tables, load pipeline, and codegen patterns.
  - Log tests run/pending; note any deviations from CL spec.

- **Cleanup/Backlog**
  - Remove reliance on bootstrap C backend paths; ensure all build/test flows are Lisp->machine code.
  - Audit hex usage across generated code and literals.
  - Clarify error handling strategy (signals vs panics) in runtime calls used by JIT.
