---
title: Separate verified compiler loads from user loads
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-11T21:47:11.209176+02:00\""
---

Problem: full native refresh fails at tools/build-fixpoint.f BF-BOOTSTRAP-STAGE because bin/hb --load seals the friend arena before evaluating generated stage2-src; the first protected package CHECKER-CERT exits E-SEAL-PACKAGE and the parent reports E-BUILD-STATUS. Cause: generated compiler payload is routed through the ordinary user-load boundary after EMIT-SEAL-FRIEND-TOKEN became universal. Fix: add an explicit build-only source mode in src/habu/habu2.f (and bootstrap parity) that loads the cold prefix without sealing, require generated build sources to emit SEAL-FRIEND at the compiler/user-driver boundary, and route build-fixpoint/hb-build internal compiler payloads through it. Acceptance: ordinary --load remains sealed and test/seal-package.f rejects protected reopens; generated stage/stdin/snapshot/maker sources certify, seal before driver code, full bootstrap reaches fixpoint, build-fixpoint tests and full owning gates pass. Files: src/habu/habu2.f bootstrap/cg/forth.fs tools/build-fixpoint.f tools/build-fixpoint-test.f tools/hb-build-lib.f src/habu/stage2.f LESSONS.md. Verify: bootstrap check-only and full install; build-fixpoint-test; hb-build-test; seal-package; host-lint; filemap-lint; full gate.
