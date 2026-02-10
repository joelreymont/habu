# Hoist Issues Tracker

Issues discovered in the [hoist](https://github.com/[...]/hoist) SSA JIT backend
that need to be fixed upstream or worked around in habu.

## Fixed

### 1. LDUR encoding bug (bit transposition)
- **File:** `src/backends/aarch64/emit.zig` (emitLdr)
- **Symptom:** Callee-saved registers not restored after calls; fib(n≥2) returns wrong values
- **Root cause:** `emitLdr` used constant `0b11110000010` (opc=00=STUR, bit21=1) instead of `0b11110000100` (opc=01=LDUR, bit21=0). Single bit transposition caused LDUR to encode as STR-register.
- **Fix:** Changed constant to `0b11110000100`
- **Commit:** `825d4853` on `codex/habu-fixes`

## Open

### 2. Constant caching violates SSA dominance across blocks
- **File:** habu `src/jit/hoist_backend.zig` (IrTranslator.cachedIconst)
- **Symptom:** When an `iconst` is first used in a then-branch block, the cached value gets reused in else-branch blocks that are NOT dominated by the then-branch. This produces invalid SSA IR (use without dominating def).
- **Workaround:** Call `preEmitConstants()` on the entire IR body in the entry block before translating, so all constants are emitted in the entry block which dominates everything.
- **Proper fix:** Hoist should either (a) detect and report dominance violations in verification, or (b) the IR builder should automatically hoist constants to the entry block.
- **Status:** Workaround applied in habu. Hoist verification currently does NOT catch this.

### 3. Parallel copy conflicts in call argument setup
- **File:** hoist `src/codegen/compile.zig` (jump handler)  
- **Symptom:** For multi-arg indirect calls (e.g., tak with 3 args), the lowering emits sequential `mov` instructions for call arguments. When source and destination registers overlap, earlier movs clobber sources needed by later movs.
- **Example:** `mov x0, x20; mov x1, x0` — the first mov overwrites x0 before the second can read it.
- **Workaround:** `fixCallArgMoves` post-pass in habu that detects BLR sequences and reorders/breaks cycles using x9 as scratch.
- **Proper fix:** Hoist should emit parallel copies correctly (using a proper parallel copy algorithm), or the register allocator should handle this.

### 4. Register allocator: no spill support
- **File:** hoist `src/regalloc/linear_scan.zig`
- **Symptom:** When more values are live simultaneously than physical registers available, allocation fails with an error instead of spilling to stack.
- **Impact:** Limits complexity of functions that can be JIT-compiled. Functions with many live variables across calls will fail.
- **Workaround:** None in habu; such functions fall back to bytecode interpreter.
- **Proper fix:** Implement spill slot allocation and reload insertion in the linear scan allocator.

### 5. No optimization passes for recursive functions
- **File:** hoist codegen pipeline
- **Symptom:** Recursive functions are compiled with `.none` optimization to avoid issues, missing out on constant folding, dead code elimination, etc.
- **Impact:** Generated code is larger and slower than necessary.
- **Workaround:** Set `.none` for recursive/looping functions in habu's `compileIr`.
- **Proper fix:** Fix whatever optimization pass bug causes issues with recursive IR and enable `.aggressive` for all functions.

### 6. No loop optimizations (LICM, strength reduction)
- **Impact:** Loops re-compute invariant values each iteration.
- **Proper fix:** Add LICM (Loop-Invariant Code Motion) pass to hoist.

### 7. Missing verification of SSA dominance
- **Symptom:** Invalid IR (values used outside dominating block) compiles silently and produces wrong machine code.
- **Proper fix:** Add dominance tree computation and SSA validation to `verification(true)` mode.
