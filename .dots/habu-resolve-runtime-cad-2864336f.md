---
title: Resolve runtime CAD effect bindings
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:26:04.306765+02:00"
blocks:
  - habu-require-maki-op-b14ccc89
  - habu-add-explicit-cad-58a05453
  - habu-v2-canonical-artifact-ee5121b4
---

Full context: op-schema effect rows are static slot declarations and must not embed per-execution parameter digests, mutable state generations, RNG positions, or device authority identities. Fix: add one checked runtime resolver that combines a sealed canonical CAD-EFFECT row with typed invocation operands, attributes, capability tokens, the row's stable semantic site path, and canonical Artifact metadata to produce a sorted semantic binding set. Parameter reads resolve immutable payload digests; state/random bind owner plus generation/sequence; IO, device, allocation, atomic, collective, and publication either bind exact authority facts or return a typed uncacheable/unresolved reason. Acceptance: weight-versus-bias and different invocation paths remain distinct even when slot numbers match; only an exact repeated atom/site/slot/semantic-fact tuple is idempotent; the same atom/site/slot resolving to different facts is a typed conflict; different atoms may resolve the same site/slot without collision; every static binding resolves exactly once or returns a typed reason; missing/stale/wrong-kind artifacts and capabilities reject; site paths derive from canonical revision/node/call structure, never an address or insertion counter; output is deterministic and no cache owner can construct or project the set privately. Files: new maki/effect-bindings.f and focused test only. Verify: mutation/property fixtures, artifact/capability negatives, Maki focused gate. Depends on mandatory op rows, explicit capability tokens, and canonical artifact envelopes.
