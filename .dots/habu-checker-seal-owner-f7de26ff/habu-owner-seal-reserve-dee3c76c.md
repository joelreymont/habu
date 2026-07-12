---
title: "Owner seal: reserve pair registry"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-12T16:18:14.005349+02:00"
closed-at: "2026-07-12T20:00:36.629726+02:00"
close-reason: Implemented sealed owner-WID registry with atomic publication, cold-reset proof, hidden emitter capabilities, source closure, process isolation, and full native gate evidence.
---

Problem: native DATA has only a flat constructor protected-WID table and cannot distinguish sealed-owner public/private roles. Acceptance: reserve count plus atomic public/private WID pair rows in the protected DATA band; add capacity constants, cold-zero initialization, preflight and role predicates; exact-capacity succeeds, one-short and duplicate reject before any store; existing constructor semantics remain valid or move through an explicitly versioned layout change. Files: src/habu/layout.f, src/habu/habu1.f, src/habu/habu2.f, focused layout/seal tests. Verify: native build/fixpoint, layout constants, injected membership/capacity tests, test/seal.f slice. Depends: none. Ownership: DATA layout, atomic pair table and native predicates only; no snapshot, AOT, bootstrap, checker grammar or owner migrations. Claim: agent=/root/seal_registry_impl workspace=.jj-ws/v2-seal-registry.
