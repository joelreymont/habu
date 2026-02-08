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
