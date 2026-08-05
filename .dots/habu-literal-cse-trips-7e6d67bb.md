---
title: Literal CSE pays a spill for the movz it saves
status: active
priority: 2
issue-type: task
blocks:
  - habu-rematerialize-constants-cdce9a24
created-at: "2026-08-05T17:12:57.771667+02:00"
---

Constant-CSE over literal materializations (habu-fold-constants-and-cbe4e25e, option (a)) is implemented and correct in shape but does NOT yet pass. Evidence, so the next attempt starts here rather than from zero; the working patch is at /private/tmp/claude-501/fold-lint/literal-cse.patch (145 lines).

THE TRANSFORM. src/compiler/native/elaborate.f EMIT-LIT is the single choke point every integer literal op reaches - the tape's own literals (EMIT-CONST), the address a created data word names (EMIT-FIXED-SYM), and the constant half of a constant-and-operation word like 1- (EMIT-CONST-OP-SYM). A block-local memo of (literal value -> ir-value-id) consulted there, cleared at all three IR-BUILD:BEGIN-BLOCK sites, collapses repeated materializations. Block-local is the whole correctness argument: an earlier op of the same block dominates every later point in it, and hir.const is pure and total, so reuse is a direct refinement. Across blocks it is a real dominance question and is deliberately not attempted.

THE FAILURE. With the memo enabled, test/compiler/native-elaborate.f throws -8042 E-IR-FUN-BOUND, from src/compiler/ir/fun.f:1044 - END-BLOCK's check that a block's argument window lies inside the minted values: 'agst BSTG-AGN @ + v IR-OP:VALUES > if E-IR-FUN-BOUND throw then'. Isolated by flipping ONLY the default: memo off, every suite green and every corpus row byte-identical; memo on, that throw. So the fault is the reuse, not the wiring.

WHAT WAS NOT ESTABLISHED. Which staging invariant skipping the op breaks. Skipping EMIT-LIT's OPEN/OPERANDS+/RESULTS+/CLOSE means no value is minted for that literal, and something in the block-argument bookkeeping expects a value count that the skip lowers. Candidate next steps: instrument BSTG-AGST/BSTG-AGN and IR-OP:VALUES around a two-literal single-block body; check whether a block whose args were staged before the skip records an argument start that the lowered value count then invalidates; and check the inline splice path, which stages a callee's tokens into the caller's current block.

THE PRIZE, MEASURED. CODEGEN-CORPUS4:CALL-FAN-BIG emits ten movz for two distinct literals (verified by disassembly: five inlined copies of C-MAD, each re-materializing 3 and 5). Collapsing them predicts 23 -> 15 instructions, chain 88 -> 56 bytes, gap-vs-clang 72 -> 40. At 56 the row is still a loss against the OLD emitter's 36, so the known-loss entry survives with a smaller number rather than retiring.

MEASURED 2026-08-05 (fold2). The premise above is WRONG in its diagnosis and the
error code was mis-attributed. There is no block-argument bound bug, no IR-layer
defect, and nothing wrong with the memo's reuse rule. What the lane actually hit
is the transform's own cost.

WHY THE ATTRIBUTION WAS WRONG. -8042 E-IR-FUN-BOUND is the generic "index at or
past the count the table records" code (lib/errors.f:580) and fun.f throws it
from more than twenty sites, not only END-BLOCK's argument-window check at
:1044. A probe planted on that exact line - printing agst, BSTG-AGN and
IR-OP:VALUES immediately before the throw - never fired, while the suite still
threw -8042. So END-BLOCK's check was never reached, and the question the dot
poses (is the argument window real, or is it counting positions where it should
count values) does not arise.

WHERE IT REALLY CAME FROM. The throw is the frozen block reader IR-FUN:FOP@
refusing an operation index, raised by the TEST, not by the compiler.
test/compiler/native-elaborate.f BUMP-BODY reads `m blk 7 F-OP` and
`m blk 8 F-OP` by absolute index and asserts the block holds exactly 10
operations. Measured through the production path with the memo switched both
ways, the bump body `BUMP CELL-A ! CELL-A @ 1+ dup CELL-A !` is:

  memo off, 10 ops, 11 values:
    mem, const, store, const, load, const, add, const, store, return
  memo on,   8 ops,  9 values:
    mem, const, store,        load, const, add,        store, return

The two repeated CELL-A address constants collapse, exactly as intended, so
index 8 no longer exists and the fixture runs off the end of the window. The
module FREEZES cleanly with the memo on, which runs the whole structural
verifier including END-BLOCK and ARGS-CK - positive evidence that the collapsed
module satisfies the block-argument invariant rather than violating it.

THE MEMO'S KEY IS COMPLETE. src/compiler/native/hir.f DEF-CONST declares
hir.const with no operands, exactly one result whose type is the schema's fixed
cell type, PURE-VALUE, and SET-TRAP false. Two hir.const operations carrying the
same integer are therefore identical in opcode, operands, attribute, result
type, purity and trap flag, so a memo keyed on the value alone cannot conflate
two different constants. That question is closed.

THE REAL BLOCKER, MEASURED. With the memo on, five of the six native suites are
green (select, migrate, inline, emit, and elaborate once its stale fixture is
corrected). test/compiler/native-chain.f RSPILL-CASE throws -8329
E-A64RA-PRESSURE. Its definition is

  : NCH-RSPILL ( n -- n ) {: a:n :} a 1 + a 2 + a 3 + a 4 + + + + 1 a < if a 1- RECURSE + then ;

compiled deliberately into a starved frame (RSPILL-REGS 4, RSPILL-SLOTS 1). The
literal 1 appears twice in the entry block, in `a 1 +` and in `1 a <`.
Collapsing them saves one movz and stretches that constant's live range across
the whole add chain and the comparison. Measured by raising the slot count: the
routine then needs TWO spilled values where the case pins exactly one
(NFIX:SPILLED, `1 T=`), and it still answers 54/54/14 correctly. So the trade
for that shape is minus one instruction, plus one spilled value - a store, a
load and a frame slot. Strictly worse.

THE PRIZE IS REAL AND SO IS THE COST. tools/codegen-compare.f over all 41 rows,
memo on, reports four improvements and no regressions, "rows the new column
costs more on: none", 0 finding(s):

  CALL-FAN-BIG  88 -> 56 bytes   (gap vs clang 72 -> 40)
  TINY-CALLEE   96 -> 80 bytes
  CALL-FAN      48 -> 44 bytes
  CALL-LOOP-3  112 -> 108 bytes

So at production register budgets the transform is a pure win; the starved frame
is where its cost shows. The two facts are the same fact: CSE's benefit IS live
range extension, and live range extension is what raises pressure. There is no
version of this transform that keeps the byte win and avoids the pressure risk.

WHAT THIS DOT NOW NEEDS. Not a fix to the memo and not a change to the IR. It
needs constant REMATERIALIZATION in the register allocator, which is the
standard resolution and makes constant CSE unconditionally safe: collapse
freely, and when the allocator would spill a value defined by a pure, operand-free,
non-trapping hir.const, re-emit the constant in front of the reader instead of
storing and loading it. That is filed as its own dot; this one stays open behind
it. Landing the CSE before it would mean either shipping a measured
pessimization or widening RSPILL-CASE's pin to match the code, and the pin is
the evidence.

The working patch (145 lines, applies clean to ee6463fc) is preserved at
literal-cse.patch on the folding lane's scratch, together with the BUMP-CASE
correction it needs.

Claim: agent=fold2 workspace=.jj-ws/habu-fold-constants-and-cbe4e25e
