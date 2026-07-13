---
title: Require Maki op effect rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.401416+02:00"
blocks:
  - habu-seal-cad-effect-49cac404
---

Full context: maki/op-registry.f records class, cost, numeric policy, arity, VJP, and reference evaluator but no semantic effect or bound slot, so registry completeness cannot establish optimizer legality. Fix: make one sealed canonical static CAD-EFFECT row mandatory for every op schema, validate all referenced operand/attribute/capability slots against schema arity/kind, include the row in schema identity, and classify the existing explicit-tensor census without hidden defaults. The schema row stores slots, never runtime digests or authority instances. Acceptance: a missing row, missing binding, wrong-kind/out-of-range slot, direct duplicate, or inconsistent PURE row rejects registration; weight and bias parameter bindings remain distinct; every current op is enumerated; changing an atom or slot changes schema identity while changing an invocation artifact does not. Files: maki/op-registry.f and focused test. Verify: registry census/mutation tests and Maki suite. Ownership: static registry schema only; runtime resolution is habu-resolve-runtime-cad-2864336f.
