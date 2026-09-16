---
title: Make the seal and engine honour owner-private primitive rows
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-16T16:10:29.929487+03:00\""
closed-at: "2026-09-16T16:10:52.825654+03:00"
close-reason: "merged into habu-honour-owner-private-0a19f45d: hazel opened the same follow-up on the integration line first"
---

Problem: an owner-private primitive row (PPRIM: with the private closer) is admitted by the checker inside its package, but a private-only row regresses two layers: the seal (src/habu/internal-mark.f IMK-CLASSIFY) classifies primitives by their bare name at top level and marks a record DNAME-INT when the arity is unknown, and the engine (src/compiler/native/dict.f:252-266) refuses checked callers of DNAME-INT records; so every boundary primitive must keep a global trusted-only row beside its private row (measured 2026-09-16 in habu-pkg-owned-prim-e08e345f). Acceptance: the seal resolves a primitive's arity through its owner-private row when no global row exists, the engine admits a checked caller of a record whose owner-private row certified it, the trusted-only rows of ffi-call-bounded and the four publisher primitives are deleted, and the private-trust fixture suite still pins refusal outside the owner. Files: src/habu/internal-mark.f, src/compiler/native/dict.f, src/core/checker.f rows, tests. Verify: test/primitive-trust.f and the owner-private suite; byte fixpoint; test/run.f green on the integrator's build. Depends: habu-pkg-owned-prim-e08e345f. Ownership: engine and compiler lanes. Claim: unassigned.
