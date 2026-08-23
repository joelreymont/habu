---
title: AOT and hb-build Linux legs are red
status: active
priority: 1
issue-type: task
created-at: "2026-08-23T11:42:23.353495+02:00"
---

Problem: the first Linux run of test/run.f (2026-08-22, recovery integration) reds four AOT/hb-build cases that never ran off DGX Spark: test/aot-wid-suite.f assert 238 'AOT data-span guard: forged span dies named, legal span boots' with 'E-UNDEFINED: AOT-DATA-SIZE' (AOT-DATA-SIZE is a variable in src/habu/aot-decl.f:148, a file baked into the engine, not a boot-prefix file); hb-build-fixtures asserts 205 and 244; the native hb-build AOT positive gate ('fork hb-build AOT bundle/data', 'hb-build AOT preseed normal-MAIN exits 0', fork worker throw rc -2105 = E-FS-IO); tools/object-image-test.f assert 2 'expected 0' in the tail-process group. PICKUP.md recorded the aot-data-span-forge Linux legs as owed (habu-the-sparse-window-3368bb76). Acceptance: Dig Protocol per case - hypothesis, prediction, falsification with the real command, cause at file:line - then the root-cause fix (no skip-logic, no Linux-only branches that hide a missing capability; if a Linux capability is missing it is named and built); each case green under bin/hb --load test/run.f on this host; a negative fixture per cause. Files: src/habu/aot-*.f, src/os/linux/*, tools/hb-build*.f, test/aot-wid-suite.f, tools/object-image-test.f (as the digs find). Verify: the four cases through their real runners; the tail and aot-positive phases of test/run.f. Depends: habu-gate-cannot-be-6d68b203 only for the full-gate proof (phase 15 must pass first); the cases run standalone before that. Ownership: AOT on Linux. Claim: agent=aot-linux workspace=.jj-ws/habu-aot-and-hb-13c99792
