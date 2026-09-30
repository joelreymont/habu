---
title: Cut hb-build gate row time
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T15:12:49.920708+02:00"
closed-at: "2026-09-30T15:13:06.432005+02:00"
close-reason: Landed 857762e0. 11 rows 326.6 s -> 159.9 s standalone. Fable review restored the install-over-existing-output check (mutation proves it) and the -1 timeout case. Native gate 504/504 rc 0, 281.2 s wall, 1984 s pooled at load 78-105 (gated at d4d16008, pushed as d8867baf after rebasing over a dots-only commit).
---

Problem: every hb-build CLI spawn in the gate compiled tools/hb-build.f's library at tier 1 (8.92 s) before reading argv, and several rows rebuilt the same programs. Acceptance: the rows preload tools/hb-build-lib.f at tier 0 and run the same HBB-MAIN on the same argv; duplicate builds are removed with every assertion kept; test/stripped-image.f and test/compiler/native-long-string-image.f still spawn the production tool. Verify: per-row seconds before and after; mutation of the install-over-existing-output check fails; full native gate.
