---
title: Start ready suites before image waits
status: closed
priority: 2
issue-type: task
created-at: "2026-10-20T14:27:35.160636+03:00"
closed-at: "2026-10-20T14:29:40.778084+03:00"
close-reason: Moved eight existing long, image-independent SUITE blocks before build-fixpoint-fixtures, longest measured rows first. All 685 exact row blocks and derived row grants are unchanged; all other row order and the final SEQ tail are unchanged. The real registry load graph and entry guard pass, as do src/lib/test/suite-test.f and test/gate-images-test.f on matching hb a02380d3/map1607423e. No full gate run or timing claim; integration measurement belongs to the lead.
---

The measured native gate holds the registry at build-fixpoint-fixtures while compiler images are building, leaving five of eight pool slots idle during elapsed 121–314 seconds. Move existing independent long SUITE blocks in test/gate-stdlib-cases.f ahead of that first image-dependent row. Preserve every row, file argument, image dependency/grant/readiness rule and SEQ group. Acceptance: compare exact row multisets and before/after order, confirm moved closures need no keyed images through the real derivation, run cheap existing registry/image behavior tests; the integrator measures the full gate. Owner: carl-suite-order worker.
