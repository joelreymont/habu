---
title: "V2 R3: type artifact evidence target ids"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T07:09:38.165756+02:00"
blocks:
  - habu-v2-r3-declare-3fcdeebb
---

Problem: artifact/evidence/target/toolchain identities are same-cell values in build, validation, and promotion records, so evidence or target provenance can be attached to the wrong artifact. Fix: migrate owner records and public APIs to CAD-KIND:artifact-id, evidence-id, target-id, and toolchain-id; validate/refine only at artifact-store and toolchain-discovery boundaries. Acceptance: wrong-artifact evidence, wrong-target artifact, and wrong-toolchain evidence reject statically; one build-validate record chain certifies; no public raw n handle remains. Files: maki/store.f, maki/report.f, maki/promotion*.f, target/toolchain owner tests. Verify: focused store/report/promotion tests, maki/test.f, typed-local diff lint. Depends: CAD kind declarations.
