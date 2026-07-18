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

Design-reference (2026-07-18, tfinite): implements `docs/effects.md` § R8-3 (planner legality table). Acceptance: the planner folds `ROW-DUP-OK?`/`ROW-CACHEABLE?`/`ROW-BARRIER?`/`ROWS-COMMUTE?` over a row's atoms so `random`/`state-write` duplication and recompute reject, an `atomic` cannot reorder past a `state-write` or another `atomic` on the same site, `host-io`/`device-launch` cannot move, and `publication` never fuses or recomputes; a resolved digest changes only binding identity, never the static legality table, and unresolved `param-read` degrades to a conservative barrier — mutation-matrix rows 20, 21, 22, 23. Correctly branches off `resolve-runtime` because it consumes resolved bindings.
