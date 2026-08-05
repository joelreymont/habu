---
title: Literal CSE trips the block-argument window check
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T17:12:57.771667+02:00"
---

Constant-CSE over literal materializations (habu-fold-constants-and-cbe4e25e, option (a)) is implemented and correct in shape but does NOT yet pass. Evidence, so the next attempt starts here rather than from zero; the working patch is at /private/tmp/claude-501/fold-lint/literal-cse.patch (145 lines).

THE TRANSFORM. src/compiler/native/elaborate.f EMIT-LIT is the single choke point every integer literal op reaches - the tape's own literals (EMIT-CONST), the address a created data word names (EMIT-FIXED-SYM), and the constant half of a constant-and-operation word like 1- (EMIT-CONST-OP-SYM). A block-local memo of (literal value -> ir-value-id) consulted there, cleared at all three IR-BUILD:BEGIN-BLOCK sites, collapses repeated materializations. Block-local is the whole correctness argument: an earlier op of the same block dominates every later point in it, and hir.const is pure and total, so reuse is a direct refinement. Across blocks it is a real dominance question and is deliberately not attempted.

THE FAILURE. With the memo enabled, test/compiler/native-elaborate.f throws -8042 E-IR-FUN-BOUND, from src/compiler/ir/fun.f:1044 - END-BLOCK's check that a block's argument window lies inside the minted values: 'agst BSTG-AGN @ + v IR-OP:VALUES > if E-IR-FUN-BOUND throw then'. Isolated by flipping ONLY the default: memo off, every suite green and every corpus row byte-identical; memo on, that throw. So the fault is the reuse, not the wiring.

WHAT WAS NOT ESTABLISHED. Which staging invariant skipping the op breaks. Skipping EMIT-LIT's OPEN/OPERANDS+/RESULTS+/CLOSE means no value is minted for that literal, and something in the block-argument bookkeeping expects a value count that the skip lowers. Candidate next steps: instrument BSTG-AGST/BSTG-AGN and IR-OP:VALUES around a two-literal single-block body; check whether a block whose args were staged before the skip records an argument start that the lowered value count then invalidates; and check the inline splice path, which stages a callee's tokens into the caller's current block.

THE PRIZE, MEASURED. CODEGEN-CORPUS4:CALL-FAN-BIG emits ten movz for two distinct literals (verified by disassembly: five inlined copies of C-MAD, each re-materializing 3 and 5). Collapsing them predicts 23 -> 15 instructions, chain 88 -> 56 bytes, gap-vs-clang 72 -> 40. At 56 the row is still a loss against the OLD emitter's 36, so the known-loss entry survives with a smaller number rather than retiring.
