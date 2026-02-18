# Lessons Learned

Hard-won patterns and anti-patterns from building Habu. **Update this file at the end of every session** with new discoveries.

> Frequency counts are from SESSION.md analysis (~102K lines, ~50 sessions).

---

## Session Notes (2026-02-18)

### Worked Well
- Tracing Maxima load with per-form names (`TRACE defun ...`) made the real blocker obvious: `db.lisp` `defun clear` failed only because preceding `defmode` setup failed.
- Reducing the failure to a minimal repro (`defmode` + `putprop` arg probe) exposed the root semantic bug: proclaimed `special` lambda params were compiled lexically, so helper callees saw `name=nil`.
- Fixing lambda-parameter special semantics generically in `src/compiler/compile.zig` (dynamic `progv` wrapper for globally proclaimed special params) restored `declare-top` behavior across Maxima macros without Maxima-specific patches.
- Adding a focused regression in `src/tests/integration.zig` (`proclaimed special lambda params are dynamically visible in callees`) locks this dynamic-scope contract.
- Adding system-only/internal keywords on `maxima-load-all` (`:habu-stop-on-error`, `:habu-required-bindings`) enabled stronger diagnostics without bending CL-facing defaults.
- Removing per-form error masking in `src/interp/repl.zig` `evalFileContentSeparateVm` (propagate parse/eval errors instead of continuing) made `(load ...)` semantics deterministic and restored reliable file-level failure accounting for Maxima loader gates.
- Locking strict load semantics with a focused regression (`src/interp/repl.zig` `loadFilePublic aborts on first form error`) prevented silent partial-file success regressions.
- Fixing `loop` parser support for `FOR ... IN ... BY ...` in `lib/stdlib.habu` removed a generic clause-gap that surfaced as `Unknown loop keyword: BY` in large Lisp packages.
- Extending `get-setf-expansion` with composed list-place updaters (`cadr`/`cddr`/`caddr`/`cdddr`/aliases) removed a high-frequency `setf: unsupported place` class for macro-heavy code.

### Did Not Work
- Chasing downstream `SIMPLE-ERROR` output first was noisy; until `defmode`/special-parameter semantics were fixed, later integrate traces were mostly secondary fallout.
- Running long `zig build test -Dtest-filter=\"...maxima...\"` invocations remained unreliable/hang-prone in this environment; short focused filters and direct scripted repros gave more deterministic signal.
- Using multiline piped REPL scripts for loader RCA gave misleading/garbled diagnostics; `habu <script-file>` probes and targeted tests were more trustworthy.

## Session Notes (2026-02-17)

### Worked Well
- Following Maxima source to the exact failing semantic operation (`mrgmac.lisp` `defc/defs/defa`: `(coerce \`(lambda ...) 'function)`) gave a generic CL fix in `lib/stdlib.habu` (`coerce-to-function`) instead of a Maxima-specific patch.
- Converting temporary root-cause traces into focused regression tests (`src/tests/integration.zig`: function-designator coercion, optional `env` lambda designator arity) preserved behavior while allowing debug instrumentation to be removed cleanly from hot compiler/VM paths.
- Aligning `lib/maxima-loader.lisp` file order with upstream `src/maxima.system` module ordering (not ad-hoc sequencing) removed dependency-order regressions (`PUTOPR`/`SPECREPCHECK` class) and gave a principled path for loader parity.
- VM mismatch tracing (`HABU_TRACE_CALL_MISMATCH=1`, `HABU_TRACE_ERROR_CONTEXT=1`) exposed a generic CL semantic bug quickly: `MAPC` was fixed-arity in `lib/stdlib.habu` and failed in Maxima `$errormsg` multi-list dispatch.
- Replacing `mapc` with variadic CL semantics (`lib/stdlib.habu`) and adding focused regression coverage (`src/tests/integration.zig`: `stdlib mapc supports variadic list dispatch`) removed the callback-arity crash class without Maxima-specific patches.
- Persisting probe results to files (`/tmp/*.result`) after non-interactive `(load "...")` runs gave stable signal where REPL output was noisy; this exposed that integrate blockers were advancing from MAPC arity into missing module chain (`m2`/`schatchen-cond` unbound when `schatc` not loaded).
- Form-level tracing (`HABU_TRACE_FORMS=1`) isolated the failing loader site to `lib/maxima-stubs.lisp` form 24 (`eval-when`) quickly.
- Cross-checking Maxima symbol state through file-based reports (`with-open-file`) avoided terminal overwrite noise and made root-cause data stable (`/tmp/maxima-subset42-report.txt`).
- Reproducing with minimal Lisp snippets (outside full Maxima load) made package bugs obvious and testable.
- Adding focused regression tests in `src/runtime/primitives/package.zig` caught real root causes:
  - stale inherited-symbol replacement in native tables,
  - inherited lookup using native exports when Lisp export tables are sparse,
  - keyword nickname handling in package creation.
- Validating with the same Maxima subset gate used by integration (`lib/maxima-loader.lisp`, 39 files) gave a concrete pass criterion: `(39 39 0 1 1 1 1 1 1)`.
- Isolating Maxima `destructuring-let` failure to a language-level repro (`let` with mixed lexical + special vars) exposed the true compiler bug quickly:
  - `(let ((a 1) (*x* 2)) ...)` leaked writes to global `*x*` instead of dynamic binding.
  - Fixing mixed special/lexical lowering in `src/compiler/compile.zig` (specials via `progv` with temp bindings) removed the `LET-MACRO-HAIR` crash path.
- Adding dedicated integration regressions in `src/tests/integration.zig` for mixed special `let` and Maxima `letmac` keeps this class of bug from regressing.
- Treating `defpackage` as a strict semantic boundary (parse and apply `:import-from` / `:shadowing-import-from` instead of ignoring them) removed cross-package symbol alias bugs without Maxima-specific rewrites.
- Loading upstream Maxima package definitions first (`lib/maxima-loader.lisp` + `maxima-package.lisp`) and using stubs only as guarded fallbacks preserved symbol/package intent across diverse source files.
- Running package-form compilation in an arena-scoped compiler context (`src/interp/repl.zig` `evalPackageForm`) eliminated persistent IR node leaks on repeated `defpackage` evaluations.
- Fixing `%shadowing-import` replacement semantics in `src/runtime/primitives/package.zig` (replace conflicting local/native entries before import) aligned behavior with CL expectations and unblocked real package forms.

### Did Not Work
- Driving long Maxima probes via non-interactive `./zig-out/bin/habu < script` in this environment was unreliable for deterministic pass/fail capture; targeted integration tests were more trustworthy for regression signal.
- Using `./zig-out/bin/habu <script-file-arg>` as a multi-form probe source was misleading in this environment; only the final top-level form was reliably observed, so probe conclusions must come from integration tests or controlled REPL eval paths.
- Assuming `mapc` was already CL-compatible because `mapcar`/`mapl` were variadic was wrong; missing variadic support in one mapping combinator can break large Lisp packages in non-obvious error-reporting paths.
- Driving large multi-form scripts by piping raw lines into the interactive REPL produced misleading output corruption; loading a script file and writing explicit probe artifacts was required for trustworthy RCA.
- Using stdlib `find-symbol` as a debugging oracle was misleading; its previous shim semantics masked package-state bugs.
- Trusting `maxima-load-all` success counters alone was misleading: `sin.lisp` can leave `MAXIMA::SININT` unbound while reporting `(ok=total, fail=0)`, so binding checks (`fboundp`) are required for critical entrypoints.
- Assuming Lisp package export hash tables mirror native exports caused false negatives in inherited symbol classification.
- Accepting keyword nicknames in validation while later calling `nameBytes` (string/symbol-only) produced delayed `TypeError` in `eval-when`, not at option parse time.
- Relying on a single long `zig build test -Dtest-filter=...` run was unreliable in this environment; targeted tests plus direct REPL gate runs were more deterministic.
- Assuming a Maxima runtime failure (`$ratsimp`) was a setf-expander bug was wrong; after dependency fixes, the failure moved and the real issue was mixed special/lexical `let` compilation semantics.
- Silently ignoring unknown/unsupported `defpackage` options in `compileDefpackage` was a shortcut that hid root causes and led to hard-to-trace runtime recursion/dispatch failures.
- Implementing `shadowing-import` by delegating to plain `importSymbols` first was incorrect when same-name local symbols already existed; it caused native symbol-table conflicts instead of required replacement.

---

## Anti-Patterns (What Goes Wrong)

### 1. "Already Exists" Discovery (793 occurrences)

The #1 time sink: implementing something that's already in the codebase.

**Examples:**
- Added duplicate array opcodes (0x73-0x78) when they already existed at 0xCF, 0x1B-0x1E
- Wrote VM handlers for make_array/aref/aset, then found existing handlers 1000 lines away
- Implemented format directives that were already working

**Rule:** Before writing ANY new code, `grep -rn` the codebase for the feature name, opcode, function name, and related keywords. Check both Zig source and stdlib.habu.

### 2. Forward Reference / Ordering Bugs (199 occurrences)

Lisp macros compile their body at definition time. If a macro calls a helper, the helper must be defined BEFORE the macro.

**Examples:**
- `defmacro` using helpers defined later in stdlib.habu → CompileError
- Moved macro definitions above helpers → broke other macros depending on the moved code
- LOOP macro helpers had cascading ordering dependencies

**Rule:** In `lib/stdlib.habu`, helper functions go ABOVE the macros that use them. When adding a new helper, check all macros below it for dependency ordering.

### 3. Arena Allocator Lifetime Bugs (385 occurrences)

The REPL resets the arena allocator between expression compilations. Any IR nodes, strings, or metadata allocated with the arena become stale pointers after the next expression compiles.

**Examples:**
- defmethod stored `body: *Ir` pointers that pointed into freed arena memory → segfault
- Fix: store function NAME strings (persistent allocator) instead of IR pointers
- Slot names from defclass allocated in arena, freed before runtime execution

**Rule:** Anything that must survive across REPL expressions MUST use `globals.allocator` (persistent), NOT `self.allocator` (arena). IR nodes, compiled chunk references, and temporary strings are arena-scoped.

### 4. Package-Qualified Name Mismatches (430 occurrences)

The compiler looks up globals using qualified names like `"CL-USER:foo"`, but generated code sometimes registers with unqualified names like `"foo"`.

**Examples:**
- defclass constructors registered as `"make-person"` but looked up as `"CL-USER:make-person"` → UnboundVariable
- Fix: added `qualifyName()` helper that prepends current package prefix

**Rule:** When generating function definitions programmatically (defclass, defstruct, defmethod), ALWAYS use `qualifyName()` or `getQualifiedName()` to match the lookup path.

### 5. Reverts and Rework (118 occurrences)

Large, multi-file changes that break tests and require full reverts.

**Examples:**
- unwind-protect error handling attempted 5+ times, always abandoned
- CLOS defmethod rewritten 3 times before finding the right abstraction (store names, not IR)
- Bignum arithmetic had repeated off-by-one bugs in carry propagation

**Rule:** Make small, testable changes. Commit after each working step. If a change touches >3 files, break it into smaller dots.

### 6. Complexity Bailouts (79 occurrences)

Starting a feature, discovering it's far more complex than estimated, then abandoning.

**Examples:**
- unwind-protect on VM errors: needs dedicated effort to handle cleanup-form execution during error propagation
- Full LOOP macro: each keyword interaction multiplies complexity
- Pretty-printer: dispatch table for every type

**Rule:** When estimated time doubles, stop. Create a focused dot with the new understanding. Don't push through with partial knowledge.

### 7. Duplicate Code / Handlers (793 occurrences, overlaps with #1)

Adding code in one location without checking if it exists elsewhere in the same file.

**Examples:**
- Two sets of array VM handlers (lines ~1190 and ~2235) in vm.zig
- Duplicate opcode definitions in opcodes.zig

**Rule:** Before adding a handler/opcode/primitive, `grep -n` the target file for the name. vm.zig is 10K+ lines — duplicates are easy to introduce.

---

## Anti-Patterns (Tooling)

### 8. sed/regex Edits on Large Files (from SESSION.md patterns)

Using sed or regex-based edits on large files frequently deletes too much, duplicates sections, or corrupts syntax.

**Rule:** Use the `edit()` tool with exact `oldText` match for surgical changes. Read the target area first with `read()` to get exact text.

### 9. Editing Without Reading First

Making assumptions about file contents based on stale context.

**Rule:** ALWAYS `read()` the target lines before `edit()`. File contents change between turns. Never assume line numbers are still accurate.

---

## Positive Patterns (What Works Well)

### 1. Test After Every Change (1333 occurrences of `zig build test`)

Run `zig build test` after every meaningful edit. Catches regressions immediately.

### 2. Read Code Before Editing

Understand the existing patterns in a file before modifying it. Check how similar features are implemented.

### 3. Helper Function Extraction

When 3+ locations share logic, extract to a function. Examples: `qualifyName()`, `getPredicateOperand()`, table-driven dispatch.

### 4. Table-Driven Dispatch

Replace if-else chains with data tables. Easier to extend, fewer typos, compiler catches missing cases.

### 5. Small Dots, Frequent Commits

Break work into dots that take <2 hours. Commit after each passing test. Use `tools/dot-finish` to enforce the build-test-commit cycle.

### 6. Store Names, Not Pointers

When crossing allocator lifetimes (arena → persistent), store string names and re-resolve at use time instead of storing raw pointers.

---

## Zig-Specific Lessons

### Arena Reset Invalidates All Pointers
The REPL's arena allocator (`self.allocator` in compile.zig) is reset between expressions. Never store arena-allocated pointers in persistent data structures.

### Switch on typeKind(), Not If-Else
Exhaustive switch catches missing cases at compile time. If-else chains silently ignore new types.

### Allocator-First Convention
`fn init(allocator: Allocator, ...) Self` — allocator is always the first parameter.

### ArrayList is Unmanaged in Zig 0.15
`var list = std.ArrayList(T){};` — pass allocator to each method call, not at construction.

### Import Once, Reference via Namespace
`const types = @import("type.zig");` then `types.Type`, `types.Primitive`. Don't import individual names.

---

## Lisp-Specific Lessons

### Macro Compilation Order
`defmacro` compiles its body immediately. All helpers used by a macro must be defined above it in the source file.

### Lisp-1 vs Lisp-2
Habu is a Lisp-1 (single namespace for functions and variables), but has some Lisp-2 features (symbol-function, fdefinition). `define` sets the value cell. Functions are looked up via global variable binding, not a separate function cell.

### defclass Slot Syntax
Correct: `(defclass person () name age)` — slots are separate top-level forms.
Wrong: `(defclass person () (name age))` — this is parsed as ONE slot with options.

### CLOS defmethod: Store Function Names
Each method compiles to a separate named function (e.g., `"foo$number"`). The generic function stores the name string, not an IR pointer. This survives arena resets.

---

## Session Workflow

### Always Create Dots Before Starting Work
No multi-step work without a tracking dot. Include file paths, line numbers, and dependencies.

### Update LESSONS.md at Session End
After completing work, add any new patterns discovered. Reference specific files and line numbers.

### Check Before Implementing
1. `grep -rn` for existing implementations
2. `read()` target files before editing
3. Check both Zig source (`src/`) and Lisp source (`lib/`)
4. Look for related opcodes, VM handlers, and compiler special forms

---

## JIT-Specific Lessons

### runMaybeJit Only Called from vm.run()
`runMaybeJit` (the JIT code check) is only called in `vm.run()`, NOT in `callFromStackAt()`. This means JIT→interpreter→JIT transitions via `callFromStackAt` never check for JIT code on the callee. Fix: `callFromStackAtFast` adds a JIT check after `doCall()`.

### ARM64 Register Map for JIT
- `x19` = sp (JIT stack pointer)
- `x20` = const_pool
- `x21` = ret_buf
- `x22` = ctx (JitContext pointer)
- `x23` = frame_base (locals accessed via `LDR x0, [x23, #offset]`)
- `x24` = stack_end

### Nested JIT Calls Need Adjusted frame_base
`runJitFn` sets `frame_base = self.stack[0..].ptr` (absolute base). For nested JIT calls, `frame_base` must be `self.stack[0..].ptr + bp` where `bp` is the callee's frame base from `self.frames[fp-1].bp`. See `runJitFnInFrame`.

### sp Recovery After Nested JIT
When JIT code runs with a non-zero frame_base, recovering `vm.sp` from `ctx.sp` requires computing the absolute offset from the stack base, not from frame_base. Use `@intFromPtr(ctx.sp) - @intFromPtr(stack_base)`.

### callFast Must Use Absolute Stack Indices
`rt.callFast` computes `fn_idx` relative to `frame_base` via `stackLen(c)`. But `callFromStackAtFast` expects an **absolute** index into `vm.stack`. For top-level JIT (frame_base == stack[0]), they're the same. For nested JIT calls (frame_base > stack[0]), must convert:
```zig
const abs_fn_idx = (@intFromPtr(c.frame_base) - @intFromPtr(c.vm.stack[0..].ptr)) / @sizeOf(Value) + fn_idx;
```
Bug manifestation: recursive functions returning wrong results (e.g., fib(10) → -7 instead of 55).

### Helper-Lowered IR Must Disable Untagged Mode
When adding IR nodes lowered through C-ABI helper calls (`make_hash`, `hash_*`, `make_string`, `arr_*`, `str_set`, `position`, `format`, `intern`), keep `translator.untagged = false` for those bodies. Untagged mode assumes fixnum-only locals; boxed/string/hash values will be corrupted if untagged remains enabled.

### Coverage Work: Add Translation + Reachability Together
JIT coverage work needs three updates in lockstep:
1. `canTranslate` / `firstUnsupportedTag` node acceptance,
2. `translate(...)` lowering implementation,
3. call-safety classification (`has_cross_calls`, untagged gating).
Skipping (3) causes post-emit/liveness issues even if translation compiles.

### JIT Tests Must Use VM Stack, Not Local Buffers
Tests that manually create `JitContext` must use `vm.stack` as the stack buffer, not a local `var stack_buf: [32]Value`. When `callFast` converts frame-relative to absolute indices, it assumes `frame_base` points into `vm.stack`. A separate buffer produces garbage indices.

### Self-Call Detection: Track Stack Depth Across Opcodes
To detect `load_global FIB; ...args...; call N` as a self-call:
1. On `load_global X` where globals[X] is a closure for the current chunk: set `self_call_depth = 0`
2. On push ops (push_nil, push_i32, load_local, etc.): increment depth
3. On binary ops (add, sub, lt, etc.): decrement depth (consume 2, push 1 = net -1)
4. On `call N` where depth == N: emit self-call
5. On anything else (jumps, pops, etc.): reset tracking to null

### Self-Call Frame Setup Must Replicate doCall
The VM's `doCall` shifts args down by 1 (overwriting closure slot): `stack[new_bp + i] = stack[new_bp + 1 + i]`. The JIT self-call must do the same, or `load_local 0` will load the closure instead of arg0.

### saved_chunk_sp Limits Recursive JIT Depth
`callFromStackAtFast` uses `saved_chunk_sp` (limited to `MAX_SAVED_CHUNKS`). Each nested call uses one slot. For recursive JIT functions, this limits call depth. Increased to 256 from 16.

### tryJitCompile: Compile-Only, No Run
When adding JIT compilation in call paths, separate "compile and cache" from "run". `tryJitCompile` should only compile and return the function pointer. The caller handles `runJitFnInFrame`. This avoids re-entrance issues where compile→run→callFast→compile creates nested compilation contexts.

### Dot Workflow
Always: `dot add` → `dot activate` → work → `tools/dot-finish`. Close activate dots immediately after activation. Never start multi-step work without a tracking dot.

---

## JIT Optimization Lessons (Session 2)

### Specialize Pass Must Preserve Lambda Fields
When the specialize pass copies a lambda IR node (because the body changed), it must copy ALL fields including `safety` and `speed`. Omitting them resets to defaults (safety=1), causing check_fixnum bytecodes even when the user declared `(optimize (safety 0))`. This was a silent performance bug — everything still worked correctly, just slowly.

### Type Declarations Don't Propagate Without Explicit Wrapping
`(declare (type fixnum n))` records the type in `global_decls` but does NOT automatically wrap variable references with `assert_fixnum`. Without explicit wrapping in the compiler's variable-reference path, the specialize pass can't prove operands are fixnum. Fix: when compiling a variable reference, check `global_decls.getTypeDecl(name)` and wrap with `assert_fixnum` if the type matches a known builtin (like fixnum).

### getTypeDecl Was a Stub Returning null
The `DeclEnv.getTypeDecl()` method was a stub (`return null`) with a comment "TEMP: bypass HashMap to avoid crash". This silently disabled all type-driven specialization. Lesson: search for `return null` and `TEMP` comments that might be masking missing functionality.

### Don't Strip assert Wrappers From Specialized Ops
When converting `add(assert_fixnum(x), assert_fixnum(y)) → fixnum_add(...)`, keep the `assert_fixnum` wrappers on the operands. They serve as runtime safety checks at safety > 0. The specialized op handles the performance (no type dispatch), while the assert handles correctness. At safety 0, the emitter skips the check anyway. Stripping asserts breaks `(the fixnum ...)` contracts — `(double "hello")` would silently produce garbage instead of erroring.

### declare Not Processed in let Scopes
`filterDeclares` was only called in lambda body compilation, not in `compileLetWithTail`. So `(let (...) (declare (type fixnum ...)) body)` silently ignored the declaration. Fix: add `filterDeclares` call before compiling let body.

### Peephole Fusion: Generate Less Code, Not Better Code
The #1 JIT bottleneck is memory stack traffic: every bytecode op pushes/pops through memory. SBCL keeps values in registers. Instead of optimizing individual stencils, fuse common bytecode sequences to eliminate intermediate memory round-trips:
- `load_local N; push_i32 K; fixnum_le; jmp_nil` → `LDR; CMP; B.cond` (3 inst, 1 memory op instead of 7)
- `load_local N; push_i32 K; fixnum_sub` → `LDR; SUB; ORR; push` (4 inst instead of ~10)
This yielded 36% improvement on fixnum_loop (83→53ms).

### B.cond Encoding for Peephole Jumps
`B.cond` instruction: `0x54000000 | (imm19 << 5) | cond`. Condition codes: EQ=0, NE=1, GE=10, LT=11, GT=12, LE=13. Invert the condition for `jmp_nil` (which branches when false): LE→GT, LT→GE, etc. Use `rel19` hole type for patching.

---

## Architecture Lessons

### Stack Machine JIT is Fundamentally Broken
A stack-machine JIT that translates each bytecode to native code will always be slow because every value round-trips through memory. Peephole fusion is a band-aid — it reduces memory traffic for specific patterns but can't fix the root cause. The right architecture is SSA-based: bytecodes → SSA IR → register allocation → native code. This is what SBCL, V8, and every serious JIT does.

### SSA Over Direct IR-to-Native
Tree-shaped compiler IR (like Habu's `Ir`) represents *source structure*. SSA represents *data flow*. For JIT compilation you need data flow because: (1) phi nodes at join points tell you which definition reaches each use, (2) def-use chains enable dead code elimination and constant propagation for free, (3) SSA liveness intervals are clean for register allocation, (4) loop-invariant code motion requires knowing what doesn't change across iterations.

### Hoist Integration
Hoist (Cranelift port in Zig) provides the full SSA pipeline: IR → Optimize (SCCP, DCE, GVN, LICM) → ISLE lowering → Register allocation → AArch64 emit. Vendored as path dependency via `build.zig.zon`. Access: `hoist_dep.artifact("cranelift").root_module`. Key APIs: `FunctionBuilder` for IR construction, `ContextBuilder` for compilation settings, `JitMem` for executable memory. Types use constants (`Type.I64`) not constructors.

### Hoist Block Params vs SSA Variables
Two ways to handle phis in Hoist: (1) block params (`setBlockParams` + `jumpArgs`) — manual but doesn't trigger SSA builder, (2) SSA variables (`declareVar`/`defVar`/`useVar`) — automatic phi insertion but requires the SSA builder to compile cleanly in the consumer's build context. Block params are safer for initial integration.

**Caveat**: Block param phis don't work correctly with hoist's current codegen. The merge block param values get assigned to wrong registers. Workaround: emit `ret` directly from both branches (no merge block). This limits if-expressions to top-level position (can't be nested inside arithmetic). Future fix: fix hoist's block param → register mapping.

### Hoist Register Allocator: Caller-Saved Handling (FIXED)
**Bug**: Hoist's linear scan allocator didn't know that calls clobber caller-saved registers (x0-x18). Values in caller-saved regs were silently destroyed after calls.

**Fix**: Added `call_positions` tracking to `LivenessInfo`. Both `computeLiveness` and `computeLivenessWithCFG` now record instruction indices of call/call_indirect/blr instructions. The allocator's `tryAllocateReg` checks `spansCall()` — if a live range spans a call, only callee-saved registers (x19-x28) are considered. Required adding `isCall()` to all backend instruction types.

**Key subtlety**: A value whose last use IS the call (it's a call argument) doesn't need to "survive" the call. The span check uses `call_pos >= start AND call_pos < end` (strict less-than on end). Using `<=` for end would incorrectly force call arguments into callee-saved regs.

### Hoist AArch64 Emitter: V-Bit Bug in STR/LDR (FIXED)
**Bug**: `emitStr` and `emitLdr` (unscaled immediate forms) had bit 26 (the V flag) set to 1, generating SIMD `STUR Dt`/`LDUR Dt` instead of integer `STUR Xt`/`LDUR Xt`. Template `0b11111000000` should have been `0b11110000000` (bit 6 in the 11-bit constant maps to bit 26 in the instruction).

**Manifestation**: Callee-saved register save/restore wrote to SIMD register D19 instead of integer register X19. The restore instruction `LDUR D19` with the wrong encoding (`opc=10, size=11`) was an UNDEFINED encoding → "Illegal instruction" trap.

**Debugging approach**: Hex-dumped JIT code, manually decoded AArch64 instructions, compared bit patterns against ARM Architecture Reference Manual. The V flag (bit 26) distinguishes integer (`V=0`) from SIMD/FP (`V=1`) in all load/store encodings.

### Hoist AArch64 Emitter: LDP Encoding Bug (FIXED)
**Bug**: `emitLdp` used template `(0b1010011 << 23)` which gives `[25:23]=011` (pre-index variant) with `L=0` (store). This generated STP pre-index instead of LDP signed-offset. Two errors in one constant:
1. Wrong variant: 011 (pre-index) instead of 010 (signed offset)
2. Missing L bit: L=0 (STP) instead of L=1 (LDP)

**Fix**: Replaced opaque bitfield constant with explicit field composition:
```zig
(0b101 << 27) | (0b010 << 23) | (0b1 << 22)
```

**Lesson**: Never use magic bit constants for instruction encoding. Compose from named fields so each bit's purpose is visible and verifiable against the architecture manual.

### Self-Pointer Patching for Recursive JIT
To emit self-recursive calls via `call_indirect`, embed a placeholder constant `0x0BADF00DDEADBEEF` as an `iconst`. After compilation, scan the generated code for the MOVZ+MOVK+MOVK+MOVK sequence matching the placeholder and patch with the actual function address. Patch BEFORE `writeExec` so the I-cache flush covers the patched code (on AArch64, D-cache writes are not visible to I-cache without explicit flush).

### Hoist Aggressive Optimization Removes Recursive Calls
With `optLevel(.aggressive)`, hoist's optimizer removes `call_indirect` instructions to functions with no observable side effects. Recursive fib calls get eliminated because the optimizer can't prove they terminate. Use `optLevel(.none)` for functions with recursive calls.

### Compiler IR vs Test IR: Symbol Representation Mismatch
**Bug**: Hoist backend unit tests used `.global_ref` for function references in self-calls, but the actual REPL compiler produces `.lit` (symbol value) for the same purpose. `detectSelfCalls` only checked `.global_ref`, so recursive functions compiled from the REPL were treated as non-recursive — the self-call was replaced with `nil`.

**Fix**: Added `isCallTargetSelf()` that checks both `.global_ref` (name match) and `.lit` (symbol value with qualified/unqualified name matching). Qualified names like `"CL-USER:MYFIB"` must match unqualified symbol names like `"MYFIB"` by checking suffix after `:`.

**Lesson**: Always test the actual compilation pipeline end-to-end, not just hand-crafted IR. The compiler's output may use different IR nodes than what you expect.

### Multiple REPL Compilation Paths
**Bug**: Hoist compilation was only wired into the stdlib loading path (`compileAndRun`) but not the interactive REPL path (`evalCapturingError`). User-defined functions with `(declare (optimize (speed 3)))` never got hoist-compiled.

**Fix**: Added `tryHoistCompileLambdas` call to `evalCapturingError` after bytecode emission.

**Lesson**: In a REPL with multiple expression evaluation paths (file loading, interactive input, eval-when), new passes must be added to ALL paths.

### Signature Ownership Double-Free
**Bug**: `errdefer sig.deinit()` + later `defer func.deinit()` double-freed signature arrays when `Function.init(sig)` consumed the sig by value. If compilation failed after func creation, both deferred ops ran.

**Fix**: Track ownership with a boolean: `var sig_owned = true; defer if (sig_owned) sig.deinit(); ... sig_owned = false; // after func takes ownership`.

### Nested Self-Calls Cause Regalloc Segfaults
**Pattern**: When a self-call's result is passed as an argument to another self-call (e.g., `(tak (tak ...) (tak ...) (tak ...))`), hoist's regalloc fails to properly spill values across nested `call_indirect` instructions, causing segfaults.

**Workaround**: Detect nested self-calls (`hasNestedSelfCalls`) and refuse to hoist-compile such functions, falling back to bytecode VM.

**Affected benchmarks**: tak (nested), NOT fib (fib passes self-call results to `+`, not to another self-call).

### Hoist Loop Phi Codegen: Three Bugs
**Root cause**: Three separate bugs conspired to make loops fail:

1. **Jump phi resolution missing** (FIXED): Hoist's AArch64 `jump` lowering emitted a bare `B` instruction without generating moves for `jumpArgs` values. When `jump block1(v7, v11)` was lowered, v7 and v11 were never moved into the registers assigned to block1's params. Fix: emit parallel copies (`mov`) before the branch for each arg→param pair.

2. **Frame layout clobbers FP/LR** (FIXED): `stackSlotOffset()` started at offset 0, which overlaps with the FP/LR save area written by `STP x29, x30, [SP, #-frame_size]!`. Stack stores at `[SP, #0]` overwrote the saved return address, causing "Bus error at address 0x15" (= 21 = the tagged fixnum 10, which was the loop limit stored over LR). Fix: start `stackSlotOffset` at `out_stack_max + 16`.

3. **stack_store lowering missing** (FIXED): The AArch64 lowerer had no case for `.stack_store`, causing `LoweringFailed`. `stack_load` was handled but not its counterpart. Fix: add `.stack_store` handler with STR instruction emission.

**Impact**: fixnum_loop 52ms → 8ms (6.5x speedup).

**Lesson**: When debugging "wrong results", don't assume a single bug. The first fix (stack_store handler) revealed the second (frame layout), which when combined with the initial approach (phi) revealed the third (missing parallel copies). Test each layer independently.

### Parallel Copy for Jump Args (SSA Phi Resolution)
In SSA-based codegen, `jump block(v1, v2)` where `block` has parameters `(p1, p2)` requires generating `mov p1, v1; mov p2, v2` BEFORE the branch instruction. This is the "parallel copy" problem — values must be moved to their target registers atomically. Simple sequential moves work when there are no circular dependencies (which is true for our case since loop variables are computed into fresh SSA values before the jump).

### blockParams() Returns Stale Pointers
`func.dfg.blockParams(block)` returns a slice into internal storage. If the DFG grows (by appending instructions or values) between creating block params and reading them, the slice becomes dangling. **Save block param values immediately** after `appendBlockParam()` into a local array instead of calling `blockParams()` later.

### End-to-End Testing Reveals Integration Gaps
Unit tests for the hoist translator worked perfectly (hand-crafted IR with `global_ref` nodes), but real REPL-compiled IR used `lit` nodes for function references. Similarly, hoist's loop tests only verified compilation, not execution. Always run the actual pipeline end-to-end before declaring a feature complete.

### Machine Code Disassembly Is Essential for JIT Debugging
When JIT code produces wrong results, dump the generated machine code and decode it instruction-by-instruction. In the phi fix, disassembly immediately revealed: (1) missing parallel copies before back-edge jumps, (2) stack stores clobbering FP/LR at SP+0. Print hex + manual ARM64 decode is faster than adding tracing to the compiler pipeline.

### Constant Folding at IR Translation Level
For tagged fixnum arithmetic where one operand is a constant, fold the tag adjustment into the constant at the IR translator level. Instead of emitting `iadd(x, tagged_n); isub(result, 1)` (3 instructions), emit `iadd(x, tagged_n - 1)` (1 instruction). This saves 2 instructions per fixnum operation with a constant operand.

### LICM via Constant Cache
Without a full LICM pass in the backend, achieve the same effect for constants by maintaining a cache (`i64 → HoistValue`) in the translator. Pre-scan loop bodies for literal values and emit them in the entry block before the loop. The SSA value is then available in all dominated blocks. Combined with `optLevel(.none)` which prevents re-materialization, this keeps loop-invariant constants in registers.

### Post-Emission Parallel Copy Fixup for Call Arguments
When a compiler backend (like hoist) emits sequential `mov` instructions for call argument setup without a parallel copy resolver, source registers can be clobbered before they're consumed. Instead of fixing the backend's regalloc (deep architectural change), post-process the emitted machine code: scan backwards from each `blr` instruction, collect the preceding `mov` instructions to ABI registers (x0-x7), and topologically sort them so that a move whose destination is still needed as a source by another move is emitted last. This approach is simple, correct, and avoids modifying the backend. The key insight: the "ready" criterion for topological sort is "no remaining move reads from my destination register."

### Stack Slot Offsets Must Account for Full Frame Layout
Stack slot offsets baked into lowered code must account for ALL frame components: FP/LR save area, callee-saved register area, and outgoing stack space. If offsets only account for FP/LR (16 bytes), they overlap with callee-saved registers saved at SP+16..SP+N. During lowering, the callee-save count isn't finalized (determined by regalloc), creating a chicken-and-egg problem. Conservative reservation (assuming max callee saves) works but wastes stack space.

### Inlining Tail-Recursive Functions as Loops
Cross-function inlining for tail-recursive callees requires converting the callee's body to a loop at the hoist IR level. Key steps: (1) Create header block with phi params for callee parameters. (2) Jump from caller to header with translated arguments. (3) Set `tco_header`/`tco_exit` and `fn_name` to callee's name. (4) Translate callee body via `translateTCOExpr` — tail calls become jumps to header. (5) Non-tail exits jump to exit block. (6) Restore caller's TCO state. This eliminated ~350K BLR/RET pairs for nqueens-safe-p, reducing nqueens(10) from 3.75ms to 3.45ms.

### TCO Exit Trampoline Elimination
Nested if-expressions in TCO context generate trampoline blocks: `block14 → block11 → block8` for each return path. Detect "simple exit" branches (literals, variable refs) and jump directly to `tco_exit` instead of through merge blocks. This reduced nqueens(10) from 3.45ms to 3.37ms and eliminated 3 blocks from the IR.

### Peephole Safety: Round-Trip MOV Detection
When detecting `MOV xA,xB; MOV xB,xA` round-trip pairs for elimination, check ALL register references (rd, rn, rm) of intermediate instructions, not just MOV sources. Non-MOV instructions (CSET, CMP, etc.) may write to or read from the intermediate register. Only NOP both MOVs when the intermediate register is truly dead between them.

### IR Deep Copy for Cross-Function Inlining
To inline a function compiled in a previous REPL form, the callee's IR must survive arena deallocation. Create a dedicated `ArenaAllocator` per compiled function, deep-copy the IR body and parameter names into it, and store the arena in `CompiledFn`. The `deepCopyIr` function only needs to handle the subset of IR nodes that pass `canTranslate`.

### coalesceMovs Only for Safe ALU Ops
The `coalesceMovs` peephole pass must only coalesce MOV instructions that follow safe ALU operations (ADD, SUB, MADD). Coalescing MOV after conditional operations (CSET, SELECT) or across control flow boundaries breaks correctness because multiple branches may write to the same destination register.

## 2026-02-08: Critical JIT Bug Fixes

### Entry Param Parallel Copy (fixEntryParamMoves)
- Hoist's regalloc emits sequential MOVs for entry block param copies: `MOV xD, xS`
- For 3+ params with circular dependencies, sequential MOVs clobber values
- Fix: proper parallel copy algorithm with topological sort + x9 scratch for cycles
- `fixEntryParamMovesAlloc` can insert extra instructions via ArrayList
- Previously, `eliminateRoundTripMovs` was incorrectly NOPing broken swap pairs
  in the entry region — now skips the entry region entirely

### coalesceMovs Cross-Branch Liveness Bug
- Post-MOV consumer scan treated branch instructions as "rd0 is dead"
- But branch targets may read rd0 (e.g., phi copies in merge blocks)
- Fix: conservatively mark rd0 as potentially live when hitting a branch
- This caused TCO functions to return wrong values (e.g., f3(a,b,c))

### Hoist LDP Rt2 Register Mismatch
- When hoist merges two adjacent loads (car + cdr) into LDP, the Rt2 register
  doesn't match the regalloc's expected register for the second value
- Workaround: always use `iadd + load offset=0` for cdr instead of `load offset=8`
- This prevents hoist from merging car/cdr into LDP
- Affected ALL functions using car + cdr (sum-list, while loops over lists, etc.)

### Untagged Mode + Cons Incompatibility
- Untagged mode works with plain i64 inside function body (params untagged at entry)
- Cons cells store TAGGED values (runtime objects read by interpreter/other functions)
- In untagged mode, storing untagged values into cons cells corrupts data
- Similarly, car/cdr return tagged values that don't mix with untagged arithmetic
- Fix: disable untagged mode for functions with cons/car/cdr (`containsLoads`)

### Key Peephole Pass Ordering
1. eliminateDeadCset
2. fixEntryParamMovesAlloc (can insert instructions)
3. fuseCmpImmediate
4. eliminateRoundTripMovs (skips entry region)
5. coalesceMovs (conservative at branches)
6. eliminateUselessBranches
7. invertBranchOverBranch
8. fixCallArgMoves (if recursive)
9. fuseMulAdd
10. fuseSelectCondition
11. eliminateLeafPrologue (if !recursive)
12. compactNops (LAST)

## 2026-02-08 (continued): JIT Performance Optimizations

### Backward Branch Coalescing for Loop Phi Copies
- `coalesceMovs` now treats backward `B` (loop backedge) as safe for rd0
  when there are no BLR/BL calls between the ALU op and the branch.
- Key insight: phi copies before a loop backedge capture rd0's value into
  mov_dst. The loop header reads mov_dst, not rd0. So rd0 is dead.
- Unsafe for loops with calls: callee may clobber registers.
- fixnum_loop improved from 0.37x to 1.08x SBCL.

### Cons Constants LICM (Loop-Invariant Code Motion)
- Inline cons uses g_alloc_ptr address (48-bit), 16, and 8 constants.
- Pre-emit these constants before the loop (via `in_loop_preemit` flag).
- ONLY for non-recursive functions — recursive functions have too much
  register pressure; adding 3 more constants causes spill issues.
- list_build improved from 1ms to 300µs (matching SBCL).
- gc_cons improved to 193µs (1.07x SBCL).

### Direct Predicate Conditions in translateIf
- oddp/evenp/zerop/consp as if-conditions emit direct I8 comparisons.
- Eliminates 3-5 instructions: tagged select + brif on tagged value.
- Pattern: `(if (oddp x) ...)` → `band(x,2); icmp ne; brif`
- remove_if improved from 700µs to 42µs (0.86x SBCL).

### Untagged Mode Incompatibilities
- Untagged mode disabled for functions with:
  - cons/car/cdr (cons cells store tagged values)
  - Primitive calls (gcd/nreverse/append/assoc expect tagged args)
  - Loads (car/cdr return tagged from cons cells)
- Each incompatibility caught by separate `contains*()` check.
- Missing check caused gcd benchmark to return wrong answer (235704 vs 278574).

### Inline GCD Blocked by Hoist Regalloc
- Euclidean algorithm as hoist loop: `while b!=0: r=a%b, a=b, b=r`
- Requires swap of phi parameters (a←b, b←r) at loop backedge.
- Hoist regalloc doesn't emit phi copies for this swap → infinite loop.
- Same fundamental issue as partial TCO phi copies.
- Fallback: C-ABI jitGcd call (3.3ms vs SBCL 0.89ms).

### Hoist LDP Register Mismatch (Root Cause)
- When hoist merges `load [x, #0]` and `load [x, #8]` into LDP, the Rt2
  register assignment doesn't match the regalloc's expected register.
- Example: regalloc assigns cdr load to x2, but LDP puts it in Rt2=x19.
- Workaround: always use `iadd + load offset=0` for cdr.

### JIT Performance Optimization Session (2026-02-08)

**Partial TCO**: Enabling TCO for functions with BOTH tail and non-tail self-calls
is safe and gives significant speedup. The key: tail calls become jumps (zero overhead),
non-tail calls remain as call_indirect. For ack: 720ms→592ms (18% faster).
Guard: when partial TCO leaves non-tail self-calls, keep `is_recursive = true`.

**Local Constants for Call-Heavy Functions**: Hoist's optimizer LICM-moves constants from
loop body to entry block (block0), forcing them into callee-saved registers since their
live ranges span call sites. Fix: skip `preEmitConstants` for TCO functions with non-tail
self-calls, and use `local_consts` flag in `cachedIconst` to emit fresh small constants
per use-site (only in call-containing blocks). Large constants (function pointers) still cached.

**Translation-Level CSE**: Hoist's optimizer can't CSE across loop iterations (even same-block
duplicate iadd). Fix: maintain a `cse_cache` mapping `(op, lhs.index, rhs.index) → result`
during translation. Clear on block switch for SSA dominance safety. Eliminated duplicate
`(+ i 1)` in fixnum_mul: 1170µs→1091µs (7% faster).

**Hoist Call_indirect Bug**: Hoist's e-graph optimizer (any opt level > .none) incorrectly
eliminates call_indirect instructions. Must use `.none` for functions with calls.
This prevents CSE, GVN, LICM from applying. Upstream hoist fix needed.

**MOV Coalescing Limits**: The post-emission MOV coalescing pass can't eliminate phi-copy
moves when the source register is consumed by another instruction between the ALU op
and the MOV. Example: `ADD x5,x0,x4; MADD x7,x5,...; MOV x0,x5` — can't coalesce because
MADD reads x5. This costs 1 extra instruction per loop iteration.

**Multiply-by-Constant Strength Reduction**: ARM64 MADD has 3-cycle latency on Apple M-series.
Replace `imul(x, const)` with shift-add sequences: `x*3 = x + (x<<1)`, `x*5 = x + (x<<2)`,
`x*(2^n) = x<<n`, `x*(2^n+1) = x + (x<<n)`, `x*(2^n-1) = (x<<n) - x`.
Hoist's ISLE lowering has `iadd(x, ishl(y, K)) → ADD Xd, Xn, Xm, LSL #K` rules, but
they don't fire due to forward lowering order (ishl lowered before iadd can absorb it).
The shift-add still wins: 2 instructions at 1+1=2 cycles vs 1 MADD at 3 cycles.
Result: fixnum_mul 1140µs→600µs (47% faster).

**LSL+ADD Fusion Anti-Pattern on Apple Silicon**: `ADD Xd, Xn, Xm, LSL #K` (fused shifted-ADD)
is ~10% SLOWER than separate `LSL + ADD` on Apple M-series. The wide OoO engine (8+ dispatch
slots) parallelizes two simple operations faster than one complex one. Don't fuse.

**Loop Rotation Blocked by Phi Copies**: Bottom-tested loops (SBCL-style) save 1 unconditional
branch per iteration. But hoist's regalloc inserts MOV instructions for phi parameter copies
on the back-edge, adding 2+ instructions that offset the savings. Needs hoist phi coalescing.

**Hoist brifArgs Parameter Bug**: `brifArgs` (conditional branch with block arguments)
doesn't correctly insert phi copies — the target block's parameter register doesn't match
the source value's register. Workaround: use separate trampoline blocks with explicit
`jumpArgs`. This adds overhead but is correct.

**Defer TCO Args After Inner Call**: For `(ack (- m 1) (ack m (- n 1)))`, computing `m-1`
before the inner call forces a callee-saved register to hold the result. Computing it
AFTER the call reuses the phi param register (still intact as callee-saved). Saves 1 STP
pair in prologue. Implemented by splitting arg translation: call-containing args first,
then simple args after.

**getFixnumLit Returns Raw Tagged Value**: In untagged mode, `getFixnumLit` returns the
raw tagged value (e.g., 7 for literal 3). Must shift right by 1 to get the actual numeric
value for strength reduction in untagged mode. Bug caused multiply-by-7 instead of by-3.

### Backend Migration + Perf Audit Session (2026-02-17)

**Dead Legacy Backend Surface**: `src/lib.zig` exported `src/ir/ir.zig` even though runtime
paths use Hoist via `src/jit/backend.zig`. Keeping dead exports preserves stale APIs and
needlessly compiles abandoned code. Remove the export and delete dead backend modules.

**Benchmark Harness Must Avoid Stdlib-Only Calls**: `bench/vm.zig` used
`(concatenate 'string ...)` without loading stdlib, causing `UnboundSymbol` in VM bench
(`src/interp/vm.zig:8825`). VM microbenches should use primitives guaranteed available in
the bare compiler/VM setup (e.g., `make-string` + `length`) or explicitly load stdlib.

**Perf Gating Requires Stable Bench Runners**: `bench-comp` currently crashes in JIT mode
on `gcd` (`src/interp/vm.zig:718` calling `CompiledFn.callFromValues`). Before optimizing
hot paths, lock down benchmark stability; otherwise perf regressions/improvements are noisy.

**Doc Drift Is a Real Performance Risk**: stale file references (`src/jit/jit.zig`,
`src/jit/stencils.zig`, `src/jit/patch.zig`, `src/jit/ctx.zig`, `src/jit/rt.zig`) mislead
optimization work and waste cycles. Keep docs path-valid against both `src/` and `../hoist/src/`.

**Post-Emit Liveness Must Model Call ABI Reads**: peephole dead-code elimination in
`src/jit/backend.zig` removed MOVZ arg setup before `blr`, because liveness treated call
boundaries as "reg dead". On AArch64, indirect/direct calls read x0-x7 (args), x8 (sret),
and `blr` also reads its target register. If that is not modeled, optimizers can turn
correct indirect calls into wrong-result or crashy code paths.

**VM GC Root Churn Drops By Using Slots Over Mirror Arrays**: `collectGarbageExtra`
in `src/interp/vm.zig` no longer builds a temporary `ArrayList(Value)` (`gc_vals`) for
frame closure/chunk roots. Using stack-local `Value` roots registered as `slots` avoids
dynamic buffer growth and copy-back indexing complexity while preserving pointer re-derive
after GC (`chunkFromValue` / `toPtr(Closure)`).

**Maxima Loader Must Not Auto-Execute At File Load**: loading a broad Maxima module set
can hit VM `StackOverflow` that is not recoverable through Lisp-level `handler-case`.
Keep `lib/maxima-loader.lisp` as a callable API (`maxima-load-all`) and avoid auto-running
the full load sequence during file import.

### Stream READ Semantics Can Invalidate Loader RCA (2026-02-17)

`lib/stdlib.habu` currently defines stream `read` by consuming the entire
stream into a string and then parsing once:
- first `(read s ...)` returns the first form
- second and later reads return `:EOF`

Evidence:
- `/tmp/read_many_target.lisp` with forms `1 2 3` produced `R1=1, R2=:EOF`.

Impact on Maxima loader debugging:
- "formwise read/eval" probes that appeared to succeed (`DONE forms=1 ok=1`)
  were not trustworthy for multi-form files because stream `read` never
  advanced past the first form.
- Removing `handler_sp/catch_sp` clobber in `evalFileContentSeparateVm` did
  not fix `db/compar/limit` load overflows and introduced new regressions
  (`mlisp` load failure), so that change was reverted.

Actionable takeaway:
- Do not use stream-`read` loops as a fallback loader path until stream `read`
  is fixed to consume one form at a time.

### Maxima Integrate Chain Needs Runtime-Callable Dependencies (2026-02-17)

`fboundp '$integrate` is not a sufficient gate for integration readiness.
With a reduced subset, `$integrate` can still fail at runtime with
`(UNBOUND-VARIABLE UnboundSymbol)` due to missing transitive call targets.

Evidence from targeted tracing:
- `TRACE unbound function: ALIAS`
- `TRACE unbound function: SININT`

Fix pattern:
- include `suprv1` (defines `alias`) and `sinint`/`sin` in the integrate subset,
  plus existing `schatc` chain (`partition`, `m2`, `schatchen-cond`).

Testing rule:
- integration gate must execute a real call
  `($integrate '((mexpt) $x 2) '$x)` in `src/tests/integration.zig`,
  not just symbol/macro presence checks.

Environment guard:
- Maxima-source fixtures can disappear or change layout under `/tmp/maxima`.
  Guard Maxima integration tests with a source-presence check
  (`/tmp/maxima/src/lmdcls.lisp`) and `error.SkipZigTest` so non-Maxima
  environments still run the rest of the suite deterministically.
- Prefer candidate-root probing (`/tmp/maxima/src/`, `/tmp/maxima/src/src/`,
  `/tmp/maxima/`) in both loader and tests to avoid path drift regressions.
