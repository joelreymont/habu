---
title: Retire 0 set-check sites
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.854598+02:00"
---

Ratchet the global-disable escape hatch to zero: every '0 set-check' site (outside the builder prefix, which staged fixpoint checking dissolves) must be retired as capabilities land, or carry an inventory class + owning capability dot. Enforce via habu-trusted-inventory-classifier gate check: set-check count monotonically non-increasing, hard-fail on new sites without a dot reference. End state: set-check exists only for the seed/bootstrap path, documented in TRUSTED.md.

## Site-by-site audit (2026-07-07, from head 2c8d4c31)

SETCHECK 11 -> 9. Full disposition:

Retired now (typed rewrite):
- tools/lint/text.f:10 and tools/check-core.f:18 (hook-install spans):
  existed only because the hook bodies call CHECK!, which the checker did
  not model. Replaced by `s" CHECK!" s" ptr u8 n -- n" TRUST` prim-axiom
  rows (owner habu-primitive-effect-axiom-1119f176); hook definitions now
  compile fully checked. Probe-proven before edit (TRUST + checked hook def
  under the baked hook, rc 0).

Queued capability - builder prefix (6 sites, engine files, annotations
specced for engine-lane routing, see report):
- src/habu/aot-closure.f:5, aot-lib.f:15, build.f:13, maker.f:8, snap.f:25
  (bare) and src/habu/hide.f:20 (TRUSTED: BFR-CHECK-OFF): all dissolve with
  habu-staged-fixpoint-src-0b5fc6e6 (+ its builder-trust conversion dep),
  whose blocked-evidence section names these exact injected boundaries.
  TRUSTED.md: hide.f:BFR-CHECK-OFF re-owned to the staged-fixpoint dot.

Queued capability - test spans (3 sites, annotated in-file):
- test/engine-suite.f:1395 (HIDX rollback churn): manual ndict/cp rollback
  without checker-registry rollback; raw-dictionary churn is the mechanism
  under test. Cross-ref habu-seal-set-check-b3676b33 (friend-latch
  migration).
- test/prop-test-core.f:182 (CHK-COMPILE-CERT) and :286 (CONFIRM-FR?):
  differential fuzzing requires unchecked compile (measure true arity;
  confirm false rejects). Already named TRUSTED: boundaries with typed
  effects; file-level row re-owned to habu-seal-set-check-b3676b33.

End state unchanged: remaining 9 sites all carry class + owning capability
dot; ratchet enforced by TRUSTED.md row counts under trusted-inventory
strict.
