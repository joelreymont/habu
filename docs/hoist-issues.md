# Hoist Issues Tracker

## Performance vs SBCL (2026-02-08)

All benchmarks: `(declare (optimize (speed 3) (safety 0)))`, ARM64 Apple Silicon.

| Benchmark | Habu Hoist JIT | SBCL 2.5.10 | Ratio |
|-----------|---------------|-------------|-------|
| fib(35) | 46ms | 52ms | **1.13x faster** |
| tak(18,12,6) x1000 | 95ms | 92ms | ~equal |
| ack(3,10) | 179ms | 373ms | **2.08x faster** |


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

### 5. `.aggressive` optimization eliminates load instructions
- **File:** hoist optimization passes
- **Symptom:** Functions with `load` instructions (e.g., car/cdr dereferencing cons cells) produce wrong code under `.aggressive` optimization. The load is eliminated and replaced with NOP, causing comparisons to use wrong values.
- **Example:** `(= (car placed) 42)` — the `load [placed+0]` is removed, comparing the pointer `placed` directly with 42.
- **Impact:** Any function with memory loads must use `.none` optimization.
- **Workaround:** `containsLoads()` detector in habu forces `.none` for functions with car/cdr.
- **Proper fix:** Fix the aggressive optimization pass to not eliminate load instructions with side effects or whose results are used.

### 6. No loop optimizations (LICM, strength reduction)
- **Impact:** Loops re-compute invariant values each iteration.
- **Proper fix:** Add LICM (Loop-Invariant Code Motion) pass to hoist.

### 7. Missing verification of SSA dominance
- **Symptom:** Invalid IR (values used outside dominating block) compiles silently and produces wrong machine code.
- **Proper fix:** Add dominance tree computation and SSA validation to `verification(true)` mode.

### 8. ~~Jump args to block params don't emit phi copies~~ [NOT A BUG]
- **Status:** RESOLVED — was misdiagnosed. The test was executing code from non-executable memory (heap buffer instead of JIT mmap), causing Bus Error. Hoist's phi copy emission IS correct for `jumpArgs` to `appendBlockParam` blocks.
- **Evidence:** Added `e2e_merge.zig` tests: simple merge, TCO factorial, 3-param TCO all pass when using `JitMem` for executable memory.
- **TCO now works in habu:** safe-p tail recursion → loop via hoist block params.

### 9. Entry param shuffle bug for .aggressive 3-param leaf functions
- **File:** hoist `src/codegen/compile.zig` or regalloc
- **Symptom:** When `.aggressive` optimization compiles a 3-param function with no calls, the register allocator may assign overlapping source/destination registers for the initial param copy. Sequential `MOV x2, x1; MOV x3, x2` clobbers x2 (originally c) before x3 reads it.
- **Example:** `(defun f (a b c) (if (= c 0) (+ a b) 0))` returns wrong values.
- **Impact:** Correctness bug for leaf functions with 3+ params under `.aggressive` optimization.
- **Workaround:** Such functions currently fall through to bytecode interpreter (they don't have `safety 0` or aren't leaf). Functions compiled with `.none` (recursive/calling) save to callee-saved regs first, avoiding the conflict.
- **Proper fix:** Use parallel copy algorithm for entry param shuffling, or ensure regalloc doesn't create overlapping assignments for ABI parameter registers.
