# Hoist Migration Plan: Remove Habu Codegen Duplication

## Problem

`src/jit/backend.zig` (~5950 lines) contains two distinct layers:

1. **IrTranslator** (~2700 lines, 49–2985): Translates Habu Lisp IR → Hoist IR using hoist's `FunctionBuilder`. This is LEGITIMATE — it's Habu-specific frontend lowering.

2. **Post-hoist peephole passes** (~2550 lines, 3395–5949): ARM64 machine code peephole optimizations operating on raw bytes AFTER hoist compiles. These DUPLICATE what hoist should be doing internally.

Additionally, hoist has a **phi copy bug**: jump instructions with block arguments emit sequential MOV instructions that can create physical register conflicts after register allocation. Hoist already has `../hoist/src/machinst/parallel_copy.zig` with a proper resolver, but it's not used in the aarch64 lowering path.

## Root Cause

Hoist's aarch64 codegen has gaps:
- No parallel copy resolution for phi moves at block boundaries
- Missing peephole optimizations (CMP+imm fusion, dead CSET elimination, MOV coalescing, etc.)
- No leaf function prologue elimination
- No MOVZ+ALU fusion

Habu worked around every gap by post-processing the raw machine code bytes — a brittle approach that's hard to maintain and test.

## Migration Plan

### Phase 1: Fix phi copy resolution in hoist (BLOCKING)

**File:** `../hoist/src/codegen/compile.zig` (jump handler, ~line 5700)

**Current code:** Emits sequential `mov_rr` instructions for jump args → block params. After register allocation rewrites vregs to pregs, these can conflict.

**Fix:** Use hoist's existing `../hoist/src/machinst/parallel_copy.zig` resolver. After `rewriteRegisters` maps vregs→pregs, scan each block's trailing MOV+B sequence, build a `parallel_copy.Move` list from the physical registers, call `parallel_copy.resolve()` with x16 as temp, and rewrite the MOV sequence.

**Specifically:**
1. In `rewriteRegisters` (aarch64 case), after rewriting all instructions, call a new `resolvePhiCopies()` function
2. `resolvePhiCopies()` iterates blocks in the VCode. For each block, finds trailing `mov_rr` sequences before a `b` instruction
3. Extracts physical register src/dst pairs from the rewritten MOVs
4. Calls `parallel_copy.resolve()` to get properly ordered moves
5. Replaces the MOV sequence with the resolved sequence (inserting extra instructions for cycle-breaking scratch moves)

**After this:** Remove `fixEntryParamMovesAlloc` and `fixCallArgMoves` from habu's backend.zig — they become unnecessary.

### Phase 2: Move peephole optimizations into hoist

Each habu peephole pass should become a hoist aarch64 backend pass. Target file: `../hoist/src/backends/aarch64/peephole.zig` (exists, currently 382 lines).

| Habu function | What it does | Hoist equivalent |
|---|---|---|
| `fuseCmpImmediate` | MOVZ+CMP → CMP-imm | ISLE pattern or peephole |
| `fuseSelectCondition` | CMP+CSET...CMP+CSEL → CMP...CSEL | peephole |
| `eliminateDeadCset` | Remove dead CSET after fused brif | peephole |
| `fuseMulAdd` | MUL+ADD → MADD | ISLE pattern |
| `eliminateRoundTripMovs` | MOV Rd,Rs; MOV Rs,Rd → MOV Rd,Rs; NOP | peephole |
| `eliminateLeafPrologue` | Remove STP/LDP for leaf functions | ABI/prologue logic |
| `compactNops` | Remove NOP instructions | peephole |
| `eliminateDeadFramePointerClear` | Remove dead MOV x29,xzr | peephole |
| `invertBranchOverBranch` | b.cond +8; b target → b.inv target | peephole |
| `fuseMovzAlu` | MOVZ+ADD/SUB → ADD/SUB-imm | ISLE pattern or peephole |
| `eliminateDeadMovz` | Remove MOVZ where dest overwritten | peephole/DCE |
| `eliminateDeadMovBeforeBranch` | Remove dead MOV before branch | peephole |
| `coalesceMovs` | OP Rd,X,Y; MOV Rz,Rd → OP Rz,X,Y | register coalescing |
| `eliminateUselessBranches` | B .+4 → NOP | peephole |
| `patchSelfCallsToBL` | BLR → BL for self-calls | linker/relocation |
| `patchCrossCallsToBL` | BLR → BL for known calls | linker/relocation |

**Priority order** (by perf impact):
1. `fuseCmpImmediate` — saves 1 insn per comparison
2. `coalesceMovs` — saves 1 insn per ALU op
3. `fuseMovzAlu` — saves 1 insn per small-constant ALU
4. `eliminateLeafPrologue` — saves 2 insns for leaf functions
5. `eliminateDeadCset` — saves 1 insn per fused branch
6. The rest (smaller impact)

### Phase 3: Move BLR→BL patching into hoist

`patchSelfCallsToBL` and `patchCrossCallsToBL` convert indirect calls (BLR) to direct calls (BL) for known targets. This should be a hoist link-time or post-emit pass, not habu-specific.

### Phase 4: Remove continuation stack workarounds

After Phase 1 fixes phi copies properly, the continuation stack optimization in `translateTCOBody` can use normal block parameters for the exit block (result + depth) without workarounds.

### Phase 5: Clean up IrTranslator

After Phases 1-3, `backend.zig` shrinks to just:
- Runtime helpers (jitCons, jitGcd, etc.) — ~200 lines
- IR analysis (detectSelfCalls, etc.) — ~200 lines  
- IrTranslator (Habu IR → Hoist IR) — ~2400 lines
- compileIrWithKnownFns (orchestration) — ~200 lines
- CompiledFn (JIT memory management) — ~100 lines

Total: ~3100 lines (down from ~5950, removing ~2850 lines of duplicated codegen).

## Verification

After each phase:
1. `cd hoist && zig build test` — all hoist tests pass
2. `cd habu && zig build test` — all habu tests pass
3. Benchmark: `(time (ack 3 11))` — no regression
4. Benchmark suite: `./tools/jit-bench` — no regression
