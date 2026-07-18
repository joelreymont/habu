---
title: Add explicit CAD capability tokens
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T13:16:45.397933+02:00"
blocks:
  - habu-persist-cad-semantic-028c0881
  - habu-linear-once-resource-4c58a7a1
---

Full context: raw random, state, atomic, publication, IO, and device primitives currently expose only ordinary stack effects; TRUSTED-ONLY and CAPREQ guard construction paths but do not give checked callers explicit typed authority. Fix: define sealed package-owned opaque capability tokens, linear for mutable or order-sensitive resources and one-shot for publication; checked wrappers consume or thread exact tokens while raw primitives remain inaccessible. Acceptance: missing authority, dup, drop, laundering, double publication, wrong resource, and exception-path leaks reject; legal scoped use certifies; capability owner, scope, generation, digest, and sequence are available only as typed inputs to the runtime semantic-binding resolver and never baked into a static op-schema row. Files: new src/cad/capability.f, focused boundary wrappers/tests, primitive census updates. Verify: linear/capability negatives, rollback, fixpoint, full native gate. Ownership: capability boundaries only.

Design-reference (2026-07-18, tfinite): implements `docs/effects.md` § R8-3 (explicit capability tokens). Acceptance: sealed package-owned opaque tokens follow the `CAPTOK:grant` precedent (`ROOT` + subset-only `ATTENUATE`, raw `n` inadmissible) — linear for mutable/order-sensitive resources (`state-write`, `atomic`, `allocation`), one-shot for `publication`; missing authority, duplication, drop, laundering, double publication, wrong resource, and exception-path leaks reject while legal scoped use certifies; capability owner/scope/generation/digest/sequence are typed inputs to the R8-4 resolver only and never baked into a static row — mutation-matrix rows 17, 18, 19.
