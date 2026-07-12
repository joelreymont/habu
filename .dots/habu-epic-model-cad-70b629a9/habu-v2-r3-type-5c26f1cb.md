---
title: "V2 R3: type fusion region owner"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T07:41:25.362360+02:00"
blocks:
  - habu-v2-r3-type-dfe5609e
  - habu-checker-seal-nominal-0b2eaece
  - habu-checker-seal-owner-f7de26ff
---

Problem: maki/fusion-plan.f stores and exposes region identities as n, allowing node/region/effect swaps through planning. Fix: make FP-RID storage and FP-RID@, FP-REGION-MEMBERS, FP-REGION-CLASSMIX, FP-NEW-REGION, FP-ADD and bounds checks carry CAD-KIND:region; keep region count numeric; use only private bounds-validated raw projections at array offsets with focused tests and TRUSTED.md rows. Acceptance: effect-as-region and node-as-region reject with qualified kinds; typed node -> typed region lookup succeeds; storage preserves family; no public region handle is n. Files: maki/fusion-plan.f, fusion-plan-test.f, fusion-mout-test.f, backward-test.f, TRUSTED.md, FILEMAP.md. Verify: focused tests, typed-local diff lint, trust-lint, maki/test.f, host-lint, filemap-lint.
