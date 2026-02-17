# JIT Loop Performance Gap Analysis

## Current State

Three loop benchmarks at ~0.5x SBCL (2x slower):

| Benchmark      | Habu (us) | SBCL (us) | Ratio | Hot insns/iter |
|---------------|-----------|-----------|-------|----------------|
| fixnum_loop   | 607       | 337       | 0.55x | 9 vs 6         |
| mul_accum     | 1384      | 751       | 0.54x | 10 vs 7        |
| nested_loop   | 619       | 321       | 0.51x | 9 vs 5         |

## Root Cause: Instruction Count

The ~2x slowdown comes from **9 instructions per loop iteration where 4 are wasted**.
Apple Silicon M-series decodes 8 instructions/cycle, so our 9-insn loop takes 2 decode
cycles vs 1 for SBCL's 5-6 insn loop.

### fixnum_loop Hot Path (9 instructions)

```
HEADER:
  1. [WASTE]   nop                  ; was B .+4 (now NOP'd but still decoded)
  2. [USEFUL]  cmp x3, x2           ; i < 1000000
  3. [WASTE]   cset w4, lt          ; dead! (eliminateDeadCset has wrong mask)
  4. [BRANCH]  b.lt body            ; taken 999999x
  5. [COLD]    b exit               ; taken 1x (not in hot path, but decoded)
BODY:
  6. [USEFUL]  add x1, x1, x3      ; acc += i (coalesced from add x4 + mov x1,x4)
  7. [USEFUL]  add x3, x3, x0      ; i += 1 (coalesced from add x5 + mov x3,x5)
  8. [WASTE]   nop                  ; was mov x1, x4 (NOP'd by coalescing)
  9. [WASTE]   nop                  ; was mov x3, x5 (NOP'd by coalescing)
  10.[BRANCH]  b header             ; unconditional back-branch
```

Wasted: 4 instructions (3 NOPs + 1 dead cset)
Useful: 3 (cmp, add, add)
Branches: 2 (b.lt + b header) — one is redundant

### SBCL fixnum-loop Hot Path (6 instructions)

```
L0:
  1. [USEFUL]  ADD R0, R0, NL0      ; acc += i
  2. [USEFUL]  ADD NL0, NL0, #2     ; i += 2 (tagged, immediate operand!)
L1:
  3. [CONST]   MOV TMP, #1966080    ; const 2000000 lo (reloaded EVERY iteration!)
  4. [CONST]   MOVK TMP, #33920     ; const 2000000 hi
  5. [USEFUL]  CMP NL0, TMP         ; compare
  6. [BRANCH]  BNE L0               ; single branch, bottom-tested
```

Note: SBCL has 2 wasted insns too (MOV+MOVK to reload the constant each iteration —
no LICM!). But it has 3 fewer total because: no dead cset, no NOP waste, bottom-tested
loop eliminates the back-branch.

### nested_loop Inner Hot Path (9 vs 5 instructions)

Habu: cmp, cset[dead], b.lt, mul, add, add, nop, nop, b = 9
SBCL: ASR, MADD, ADD, CMP #imm, BNE = 5

SBCL advantages: MADD fused multiply-add, CMP immediate (1000*2=2000 fits uimm12),
bottom-tested loop, no dead cset, no NOPs.

---

## Required Changes (in priority order)

### Fix 1: Correct the dead CSET elimination mask (trivial)

**File:** `src/jit/backend.zig` — `eliminateDeadCset()`
**Effort:** 1 line change
**Saves:** 1 instruction per loop iteration

The mask `0x1A800000` is wrong. CSET is an alias for CSINC (op2 bit 10 = 1).
Actual encoding: `0x1A9FA7E4 & 0xFFE00C00 = 0x1A800400` ≠ `0x1A800000`.

```zig
// WRONG:
const is_cset = (insn1 & 0xFFE00C00) == 0x1A800000;
// RIGHT:
const is_cset = (insn1 & 0xFFE00C00) == 0x1A800400;
```

After fix: dead cset becomes NOP → removed by compaction (Fix 3).

### Fix 2: Branch inversion peephole (easy, ~15 lines)

**File:** `src/jit/backend.zig` — new function `invertBranchOverBranch()`
**Effort:** ~15 lines
**Saves:** 1 instruction per loop iteration

Detect pattern: `b.cond .+8; b target` (conditional skip over unconditional branch).
Replace with: `b.inv_cond target; nop`.

This eliminates the `b exit` instruction from the hot path. The loop header currently
emits both `b.lt body` (forward to body) and `b exit` (to exit block). After inversion:
`b.ge exit` (to exit, usually not-taken) followed by NOP (or fall-through to body).

```
Before:                         After:
  cmp x3, x2                     cmp x3, x2
  b.lt body    ; +8 to body      b.ge exit    ; inverted, cold
  b exit       ; hot path waste   nop          ; removed by compaction
```

ARM64 condition code inversion table:
- eq↔ne, lt↔ge, gt↔le, lo↔hs, mi↔pl, vs↔vc, hi↔ls, al→nv

### Fix 3: NOP compaction pass (medium, ~80 lines)

**File:** `src/jit/backend.zig` — new function `compactNops()`
**Effort:** ~80 lines
**Saves:** 2-4 NOPs per loop iteration (biggest single win)

After all peephole passes (cset elim, coalescing, branch inversion, B.+4 elim),
remove all NOP instructions and fix branch offsets.

Algorithm:
1. Build `old_pos[i] → new_pos[i]` mapping (skip NOPs)
2. Compact: copy non-NOP instructions to output, track new positions
3. Fix branches: for each B/B.cond/CBZ/CBNZ/TBZ/BL instruction:
   a. Decode relative offset → absolute target (old_pos)
   b. Map target through position table to new_pos
   c. Map source through position table to new_pos
   d. Encode new relative offset
4. Truncate buffer

Branch encoding formats to handle:
- `B imm26`:    `0x14000000 | (offset & 0x3FFFFFF)`, offset in bits 25:0
- `B.cond`:     `0x54000000 | (offset << 5) | cond`, offset in bits 23:5
- `BL imm26`:   `0x94000000 | (offset & 0x3FFFFFF)`
- `CBZ/CBNZ`:   `0x34/0x35 << 24 | (offset << 5) | Rt`, offset in bits 23:5
- `TBZ/TBNZ`:   `0x36/0x37 << 24 | (offset << 5) | Rt`, offset in bits 18:5

Edge cases:
- Branch target lands on a NOP → map to next non-NOP position
- Self-pointer constant (`0x0BADF00DDEADBEEF`) is data, not a branch — unaffected
  by compaction since patching scans for byte pattern after all passes

### Fix 4: Loop rotation in IR generation (hard, ~80 lines in translateLoop)

**File:** `src/jit/backend.zig` — modify `translateLoop()`
**Effort:** ~80 lines (restructure IR block generation)
**Saves:** 1 branch per loop iteration (the unconditional back-branch)

Current IR structure (top-tested):
```
block0:           jump header(init_vals)
header(phis):     cond = icmp; brif cond, body, exit
body:             compute; jump header(updated_vals)
exit:             return
```

Rotated IR structure (bottom-tested with pre-check):
```
block0:           cond = icmp(init_vals); brif cond, body, exit
body(phis):       compute; cond = icmp(updated); brif cond, body(updated), exit
exit(result_phi): return
```

Benefits:
- Eliminates unconditional back-branch (body's brif goes directly to body)
- One basic block in hot path instead of two → better branch prediction
- The condition is duplicated but the pre-check runs once

Implementation:
1. In `translateLoop`, emit the condition check in the current (pre-header) block
2. Use `brifArgs` to branch to body with init_vals or to exit
3. Body block has phi params for loop vars
4. At end of body, re-evaluate condition with updated vars
5. Use `brifArgs` to branch to body(updated) or exit(result)
6. Exit block has phi param for the result

Complication: the condition IR tree (`cond_ir`) must be translatable twice — once in
pre-header and once in body. This is safe since `translate()` emits fresh SSA values.
The loop variables must be updated before the second condition evaluation.

After this change, fixnum_loop becomes:
```
body:
  add x1, x1, x3     ; acc += i
  add x3, x3, x0     ; i += 1
  cmp x3, x2          ; i < 1M?
  b.lt body           ; loop (single branch!)
```
4 instructions per iteration. Faster than SBCL (6 instructions).

### Fix 5: MADD fusion for `acc += x * y` pattern (medium, ~30 lines)

**File:** `src/jit/backend.zig` — modify `translateFixnumAdd()`
**Effort:** ~30 lines
**Saves:** 1 instruction for multiply-accumulate patterns (nested_loop)

Detect IR pattern: `iadd(var, imul(a, b))` or `iadd(imul(a, b), var)`.
Emit ARM64 MADD instead of separate MUL + ADD:
```zig
// In translateFixnumAdd, check if either operand is an imul:
if (left.* == .fixnum_mul or left.* == .mul) {
    const mul_left = try self.translate(left.left);
    const mul_right = try self.translate(left.right);
    const rhs = try self.translate(right);
    return try self.b.madd(I64, mul_left, mul_right, rhs);  // needs builder support
}
```

Requires adding `madd` to the hoist IR builder or emitting it via the existing
`imul` + `iadd` with a pattern that hoist's ISLE rules can fuse. Currently hoist's
ISLE has no MADD fusion rule, so this would need a hoist change too.

Alternative: post-pass at machine code level. Detect `MUL Rd, Rn, Rm; ADD Rd2, Rd, Ra`
and replace with `MADD Rd2, Rn, Rm, Ra; NOP`.

---

## Expected Results After All Fixes

### After Fix 1 alone (cset mask):
- fixnum_loop: 9 → 8 insns/iter (the dead cset becomes one more NOP)
- No timing change until NOP compaction is added

### After Fixes 1-3 (cset + branch inversion + NOP compaction):
fixnum_loop hot path becomes:
```
  cmp x3, x2          ; compare
  b.ge exit            ; cold exit (inverted from b.lt + b exit)
  add x1, x1, x3      ; acc += i
  add x3, x3, x0      ; i += 1
  b header             ; back-branch
```
5 instructions per iteration. Expected: ~380us (vs SBCL 337us) = **0.88x SBCL**.

### After Fixes 1-4 (+ loop rotation):
fixnum_loop hot path becomes:
```
body:
  add x1, x1, x3      ; acc += i
  add x3, x3, x0      ; i += 1
  cmp x3, x2           ; compare
  b.lt body            ; loop
```
4 instructions per iteration. Expected: ~300us = **1.12x faster than SBCL**.

### After all fixes (+ MADD):
nested_loop inner hot path becomes:
```
body:
  madd x6, x4, x7, x6  ; acc += i * j (fused!)
  add x7, x7, x3        ; j += 1
  cmp x7, x1            ; j < 1000?
  b.lt body             ; loop
```
4 instructions per iteration (vs SBCL's 5). Expected: ~250us = **1.28x faster than SBCL**.

---

## Implementation Priority

1. **Fix 1** (cset mask) — 5 minutes, prerequisite for Fix 3
2. **Fix 2** (branch inversion) — 30 minutes, prerequisite for Fix 3
3. **Fix 3** (NOP compaction) — 2 hours, biggest impact
4. **Fix 4** (loop rotation) — 3 hours, gets us past SBCL
5. **Fix 5** (MADD fusion) — 2 hours, helps nested_loop specifically

Fixes 1-3 should close most of the gap (~0.88x SBCL).
Fix 4 should make us faster than SBCL on all loop benchmarks.
Fix 5 is a bonus for multiply-heavy code.

---

## What We Don't Need to Change in Hoist

All fixes are in our post-passes or IR generation. No hoist upstream changes needed.

However, these hoist issues would give even better codegen if fixed:
- **Register coalescing in linear_scan.zig**: Allocate phi params and computation
  results to same register → eliminates need for our coalesceMovs post-pass
- **ICmp sinking**: Skip materializing icmp when only used by brif → eliminates
  dead cset entirely at source → no need for our cset elimination pass
- **Block layout**: Place fall-through block after brif → eliminates B .+4 at source
- **ADD immediate**: Use `ADD Xd, Xn, #imm` for small constants → eliminates register
  for constant 1 (saves one iconst + one register)

These are nice-to-haves that our post-passes work around.
