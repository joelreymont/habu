---
title: Define finite CAD effect rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.390552+02:00"
---

Full context: src/core/checker.f and maki/op-registry.f have no finite semantic effect vocabulary, so pure, parameter read, state write, random, host IO, device launch, atomic, collective, allocation, and publication operations are indistinguishable for cache, fusion, and recompute legality. Fix: add a checked package-scoped canonical effect row with a closed mask plus sorted duplicate-free bindings to explicit operand or attribute slots; require each non-pure bit to bind semantic authority and define conservative duplication, commute, and cache truth tables. Acceptance: incomplete or duplicate bindings reject; pure has no binding; immutable parameter reads key their snapshot digest; every atom and legality table has focused tests. Files: new src/cad/effect.f, focused test, docs/effects.md. Verify: standalone effect tests, host/filemap/dot lints. Ownership: vocabulary and row algebra only; no checker persistence or Maki registry migration.
