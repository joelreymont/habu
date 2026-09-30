---
title: Cut the rows that still take 10 s or more
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T16:51:16.837453+02:00"
---

Problem: after rounds 1-3, 46 rows of 10 s or more made up 1804 s of 2134 s pooled (gate 6, load about 100): build-fixpoint-snapshot 170 s, whitebox-engine-build 158 s, app-image 105 s, build-fixpoint-fixtures 103 s, hb-build-aot 88 s, compiler-native-long-string-image 86 s, field-proj-boundary 72 s, native-window-owner 68 s, prop 57 s, hb-build-retain 55 s, stripped-image 53 s. Acceptance: each such row is measured on a fresh gate log, its time is attributed (build, spawn, wait, cases), and the waste that is not the behavior under test is removed without losing an assertion; old and new trees are timed back to back on the same host. Files: the row files and test/gate-stdlib-cases.f. Verify: full suite; per-row times before and after. Measured 2026-09-30 on d40cc36d (gate A, load 6 to 40): 37 rows of 10 s or more make up 1146 s of 1461 s pooled. The hb-build rows do not spawn the tool repeatedly; they build in process, and each build pays a maker child that compiles the linker (5 to 6 s) or an app-build child (4.6 s). Depends: habu-run-hb-build-6acfec10 for the rows that launch tools/hb-build.f. Ownership: those rows. Claim: unassigned.
