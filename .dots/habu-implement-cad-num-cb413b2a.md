---
title: Implement CAD-NUM closed arithmetic
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T14:14:22.888354+02:00\""
---

Full context: MODEL-CAD-V2-PLAN.md B5.2 defines the complete admitted role algebra. Fix: add lib/cad-num-arithmetic.f package CAD-NUM implementing exactly ADD-BYTES/ITEMS/CELLS, typed offset/index advance/retreat/distance, MUL-ITEMS, SCALE-BYTES/CELLS, total positive-divisor DIV/REM for bytes/items/cells, cell-byte conversions, align-up, and alignment predicates. Omitted/reversed/cross-unit combinations remain unavailable. Acceptance: every table row has the exact zero/safe-max/first-overflow/underflow/misalignment cases named in B5.2; correct-signature controls and role-swap negatives; no generic raw n arithmetic or extra overload. Files: lib/cad-num-arithmetic.f, lib/cad-num-arithmetic-test.f. Verify exact tests, typed-local/refine/trust/host lints. Depends on scalar roles.

Claim: agent=cadarith workspace=.jj-ws/fable-cadnum
