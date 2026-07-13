---
title: Enforce effect-aware fusion and recompute
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.404808+02:00"
blocks:
  - habu-resolve-runtime-cad-2864336f
---

Full context: maki/fusion-plan.f considers class, use count, movement, backend, and resource budgets but ignores state, random, atomic, IO, device, allocation, and publication effects. Fix: consume mandatory sealed op rows plus their resolved invocation bindings before class/backend legality; allow duplication and recompute only for PURE or immutable successfully digest-bound parameter reads; conservatively barrier unresolved or non-read-only effects and emit a structured effect-barrier split reason. Acceptance: unresolved parameter reads, random, and state duplication reject; atomic cannot reorder across state writes; publication never fuses or recomputes; changing a parameter digest changes binding identity without changing the static legality table; existing pure fixtures remain byte-identical; diagnostics name effect, stable site, slot, and bound resource. Files: maki/fusion-plan.f and focused tests. Verify: mutation negatives, fusion tests, Maki suite, host/filemap lints. Ownership: planner legality only; depends on runtime resolution rather than assuming a static slot is immutable.
