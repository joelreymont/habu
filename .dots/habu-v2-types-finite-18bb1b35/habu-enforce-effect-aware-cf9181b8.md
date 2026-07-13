---
title: Enforce effect-aware fusion and recompute
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.404808+02:00"
blocks:
  - habu-require-maki-op-b14ccc89
---

Full context: maki/fusion-plan.f considers class, use count, movement, backend, and resource budgets but ignores state, random, atomic, IO, device, allocation, and publication effects. Fix: consult mandatory op effect rows before class/backend legality; allow duplication and recompute only for pure or immutable digest-bound parameter reads; conservatively barrier non-read-only effects and emit a structured effect-barrier split reason. Acceptance: random and state duplication reject; atomic cannot reorder across state writes; publication never fuses or recomputes; existing pure fixtures remain byte-identical; diagnostics name effect and bound resource. Files: maki/fusion-plan.f and focused tests. Verify: mutation negatives, fusion tests, maki suite, host/filemap lints. Ownership: planner legality only.
