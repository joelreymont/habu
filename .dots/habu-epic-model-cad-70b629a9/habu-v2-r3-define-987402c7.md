---
title: "V2 R3: define toolchain identity owner"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T07:39:30.272976+02:00"
blocks:
  - habu-checker-seal-nominal-0b2eaece
  - habu-checker-seal-owner-f7de26ff
---

Problem: CAD-KIND:toolchain-id is declared but no semantic toolchain descriptor or owner API exists; lib/ptx/toolchain.f only resolves PTXAS paths and emits diagnostics, while schedule keys serialize display text. Fix: add package TOOLCHAIN in maki/target/toolchain.f with immutable compiler/driver/version/config descriptors, canonical digest, validated private allocation/refinement to CAD-KIND:toolchain-id, typed lookup/projections, and an explicit adapter from audited PTXTC discovery facts; no public raw n conversions. Acceptance: target/toolchain swaps reject; unknown or incomplete discovery facts fail closed; version/config changes produce distinct identities; canonical round-trip preserves family; every private refinement has a focused test and TRUSTED.md row. Files: maki/target/toolchain.f, toolchain-test.f, lib/ptx/toolchain.f/test only at the adapter seam, maki/sched-key.f/test, maki/test.f, FILEMAP.md, TRUSTED.md, docs/model-cad.md. Verify: focused toolchain/PTXTC/sched-key tests, typed-local diff lint, trust-lint, maki/test.f, ptx-stdlib slice, host-lint, filemap-lint.
