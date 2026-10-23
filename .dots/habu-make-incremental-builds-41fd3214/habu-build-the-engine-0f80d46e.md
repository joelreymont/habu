---
title: Build the engine through whole-file reuse
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T17:12:02.956045+03:00"
blocks:
  - habu-restore-owner-state-785f1c30
---

Problem: the engine build (tools/native-build-core.f LOAD-TARGET via NDEF-CACHE:RUN, checked-prefix BORROW) must use file objects for every engine file. Acceptance: cold then warm engine builds give byte-identical image and map; two-generation convergence; full native suite; files refused by the compile-time read rule are listed in the build report. Verify: hb-build --engine cold/warm, bin/hb --load test/run.f, tools/two-generation-build.f. Design: docs/architecture.md "Builds"; working notes ~/.cache/tmp/carl-file/design.md. Depends: owner-state dot. Claim: unassigned.
