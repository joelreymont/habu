---
title: Require Maki op effect rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.401416+02:00"
blocks:
  - habu-define-finite-cad-0bdf52ad
---

Full context: maki/op-registry.f records class, cost, numeric policy, arity, VJP, and reference evaluator but no semantic effect or bound capability, so registry completeness cannot establish optimizer legality. Fix: make one canonical CAD effect row mandatory for every op schema and include every binding in schema identity and cache inputs; classify the existing explicit-tensor census without hidden defaults. Acceptance: a missing row, missing binding, duplicate binding, or inconsistent pure row rejects registration; every current op is enumerated; changing a bound snapshot digest changes the exact key. Files: maki/op-registry.f and focused test. Verify: registry census/tests and maki suite. Ownership: registry schema only.
