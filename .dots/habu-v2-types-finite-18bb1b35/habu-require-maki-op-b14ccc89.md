---
title: Require Maki op effect rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.401416+02:00"
blocks:
  - habu-seal-cad-effect-49cac404
---

Full context: maki/op-registry.f records class, cost, numeric policy, arity, VJP, and reference evaluator but no semantic effect or bound slot, so registry completeness cannot establish optimizer legality. Fix: make one sealed canonical static CAD-EFFECT row mandatory for every op schema, store it only through the typed nominal handle path, validate all referenced operand/attribute/capability slots against schema arity/kind, derive schema identity from canonical row contents rather than the process-local handle, and classify the existing explicit-tensor census without hidden defaults. The schema row stores slots, never runtime digests, authority instances, raw arena handles, or allocation-order identity. Acceptance: a missing row, forged/stale/cross-owner handle, raw variable/create/constant storage, missing binding, wrong-kind/out-of-range slot, direct duplicate, or inconsistent PURE row rejects registration; weight and bias parameter bindings remain distinct; every current op is enumerated; changing an atom or slot changes schema identity while changing the local handle, insertion order, allocation order, or invocation artifact does not; fresh-process schema identity is stable. Files: maki/op-registry.f and focused test. Verify: registry census, nominal-storage, canonical-identity, fresh-process replay, and mutation tests plus the Maki suite. Ownership: static registry schema only; runtime resolution is habu-resolve-runtime-cad-2864336f.
