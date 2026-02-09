# Lessons Learned

Hard-won patterns and anti-patterns from building Habu. **Update this file at the end of every session** with new discoveries.

> Frequency counts are from SESSION.md analysis (~102K lines, ~50 sessions).

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
