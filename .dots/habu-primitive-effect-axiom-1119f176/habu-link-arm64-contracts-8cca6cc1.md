---
title: Link ARM64 contracts to primitive rows
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T16:05:05.312800+02:00\\\"\""
closed-at: "2026-07-21T17:59:03.794049+02:00"
close-reason: "landed 39ac7cc3: PRIM-LINK checked query binds ARM64 contracts to PES axiom rows by stable identity; both-direction proof in test/prim-link-test.f; census 3774"
---

Full context: typed generated-machine effects need a stable link from each emitted primitive/callable contract to the checker primitive-effect row without depending on mutable symbol order or the entire trust-owner lifecycle. Fix: expose a package-scoped checked query keyed by stable defining-package plus row identity, reject missing/duplicate/stale links, and let the ARM64 effect schema consume that query. Acceptance: every linked primitive resolves one immutable stack-effect row; wrong package, duplicate spelling, row mutation, or unknown primitive rejects; a focused ARM64 contract probe consumes the query; no global prefix API. Files: src/core/checker.f, focused axiom-registry test, docs/effects.md; source-list/bootstrap files only if a new prefix unit is unavoidable. Verify: checker/prop/public-signature tests, bootstrap/fixpoint, typed-local lint, full native gate. Depends on primitive row ratchet and proof recipes; serialize after active checker owners.

Claim: agent=armlink workspace=.jj-ws/fable-armlink machine=spark (owns linking ARM64 contracts to primitive rows)
