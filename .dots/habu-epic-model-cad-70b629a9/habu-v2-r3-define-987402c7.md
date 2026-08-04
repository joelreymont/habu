---
title: "V2 R3: define toolchain identity owner"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T07:39:30.272976+02:00"
blocks:
  - habu-checker-seal-nominal-0b2eaece
  - habu-checker-sealed-destructure-d967fc03
---

Problem: CAD-KIND:toolchain-id is declared but no semantic toolchain descriptor or owner API exists; lib/ptx/toolchain.f only resolves PTXAS paths and emits diagnostics, while schedule keys serialize display text. Fix: add package TOOLCHAIN in maki/target/toolchain.f with immutable compiler/driver/version/config descriptors, canonical digest, validated private allocation/refinement to CAD-KIND:toolchain-id, typed lookup/projections, and an explicit adapter from audited PTXTC discovery facts; no public raw n conversions. Acceptance: target/toolchain swaps reject; unknown or incomplete discovery facts fail closed; version/config changes produce distinct identities; canonical round-trip preserves family; every private refinement has a focused test and TRUSTED.md row. Files: maki/target/toolchain.f, toolchain-test.f, lib/ptx/toolchain.f/test only at the adapter seam, maki/sched-key.f/test, maki/test.f, TRUSTED.md, docs/model-cad.md. Verify: focused toolchain/PTXTC/sched-key tests, typed-local diff lint, trust-lint, maki/test.f, ptx-stdlib slice, host-lint.

RECOVERY POINTER (forensic adjudication 2026-07-19): retired workspace v2-toolchain tip 817a1b8d already implements this dot's exact specification - maki/target/toolchain.f (544 lines, package TOOLCHAIN: immutable PRODUCT disc with compiler-path/version, driver-name/version, config; 16-hex canonical digest; epoch/generation-packed CAD-KIND:toolchain-id; INTERN/ROW>ID/ID>ROW; discovery-fact adapter; named errors E-FACT/KIND/CAP/ID/DIGEST/MISS/COLLIDE/EPOCH) plus toolchain-test.f. The commit is kept un-abandoned as the pointer. It forked before the mature TARGET registry pattern landed in maki/target/target.f, so recover the design and re-derive against that sibling pattern rather than rebasing as-is.
