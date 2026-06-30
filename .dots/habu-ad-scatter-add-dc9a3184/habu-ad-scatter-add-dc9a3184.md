---
title: AD scatter-add default for LOAD adjoints
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:00:43.571312+02:00"
---

Decomposes ad-reverse. autograd.md Memory-adjoints (review-corrected): the effect system does NOT track grid read-multiplicity, so the AD pass must NOT guess. scatter-add (red.global.add) is the CONSERVATIVE DEFAULT for every LOAD adjoint; plain-store is opt-in only behind a proven-read-once witness, never inferred. A forward reading an input across multiple blocks whose backward uses plain store must be rejected (or lower to scatter-add). Tracking read-once soundly is a first-class checker capability (substructural/affine effect) - dot it separately.
- Files: src/arch/ptx/ad.f LOAD adjoint lowering; new dot owed for the substructural effect.
- Verify: multi-block fan-in backward uses scatter-add (gradcheck-correct accumulation); plain-store-without-witness is rejected.
- Dep resolved: red.global.add is verified on sm_87 in docs/autograd.md and tools/ptx/redadd-device-test.f.

2026-06-30 local checkpoint: child `habu-ad-scatter-add-05cac9cd` landed the local typed primitives and VJP default: `LOAD` now emits `SCATTER-ADD`, `ROW-LOAD` emits `ROW-SCATTER-ADD`, and both lower to `red.global.add.f32` in PTX text tests. Remaining parent work: zed/Orin device gradcheck for accumulation and the checked read-once/affine witness dot (`habu-read-once-gradient-20ae3d1a`) before any plain-store optimization.
