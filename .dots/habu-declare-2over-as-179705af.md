---
title: Declare 2over as an HIR rename
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T00:19:55.865374+02:00"
---

Full context: flagged by agent renames 2026-07-31. hir-word.f INPUT-MAX/PICK-MAX were explicitly sized for 2over (consumes 4, puts back 4 then picks 3 2 - verify: 2over ( a b c d -- a b c d a b ) consumes 4, picks bottom-first 3 2 1 0 3 2) but it is not declared. Declare it with the derivation written out like nip/rot, an elaborator fixture on a non-commutative body asserting zero added operations and operand identity, and mutation-prove each pick index. Small leaf; do before the optimizer assumes the full rename set.
