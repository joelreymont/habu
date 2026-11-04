---
title: Build the engine through whole-file reuse
status: closed
priority: 1
issue-type: task
created-at: "2026-10-22T17:12:02.956045+03:00"
closed-at: "2026-10-31T20:52:53.154164+02:00"
close-reason: "Landed in lane 11 4b54ed21: 160/164 engine files reused, warm == cold byte for byte, two-generation rc 0. The compiled-paths report is gate row 2ec826c7's acceptance."
blocks:
  - habu-restore-owner-state-785f1c30
---

Problem: the engine build (tools/native-build-core.f LOAD-TARGET via NDEF-CACHE:RUN, checked-prefix BORROW) must use file objects for every engine file. Acceptance: cold then warm engine builds give byte-identical image and map; two-generation convergence; full native suite; files refused by the compile-time read rule are listed in the build report. Verify: hb-build --engine cold/warm, bin/hb --load test/run.f, tools/two-generation-build.f. Design: docs/architecture.md "Builds"; working notes ~/.cache/tmp/carl-file/design.md. Depends: owner-state dot. Claim: unassigned.
