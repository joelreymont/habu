---
title: Hoist loop-invariant values out of the body
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T07:21:39.129719+02:00"
---

COUNT-DOWN's emitted loop holds mov x2,#0 INSIDE the body and compares against it each turn (loops lane 2026-08-10, re-attribution: its 20-byte gap vs clang's csinc is mostly a missing compare-immediate fold - habu-compare-against-a-da4cc639 owns that - plus loop-invariant code motion). A value defined in the loop from loop-invariant operands wants defining once in the preheader. Home: derive whether combine.f's within-block shape can see it (it cannot move across blocks today) or whether the new HIR pass from habu-close-the-loops-1571fb6f is the seat - the loop structure is visible there. MANY-LOCALS' 7-add invariant chain is the same class (its closed-forming already hoists it; a general LICM covers non-closed-formable loops). Acceptance: COUNT-DOWN's residual gap after the cmp fold measured and reduced; no row regresses; direct-refinement evidence. Files: the closed-forming pass's home. Depends: habu-close-the-loops-1571fb6f (shares the pass).
