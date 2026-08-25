---
title: State what a multi-successor terminator hands its successors
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T11:06:57.239436+02:00"
---

src/compiler/ir/verify.f SUCCARGS-CK checks the operand count and types against the destination's block arguments only when a terminator has ONE successor, and says why: with more than one, the operation model cannot say which operand belongs to which destination. The native control-flow slice made that gap load-bearing - a64.cbz and hir.brz carry one operand that is NOT a block argument and both of their successors must take no arguments at all - and nothing checks the second half today. Wanted: either a schema declaration that separates a terminator's own operands from its successor arguments, or, as the smaller statement, a verifier clause that a terminator with more than one successor may only name successors with no block arguments. Until then the rule is enforced only by the elaborator that builds it.
