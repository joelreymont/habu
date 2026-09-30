---
title: Reshape gate pool rows around the image holds
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T15:12:49.930723+02:00"
closed-at: "2026-09-30T15:13:06.441117+02:00"
close-reason: "Landed 3009737a. aot-positive bundle 27.95 s and preseed 27.76 s run in the pool instead of ~28 s alone in the sequential group. Fable review: accept, no findings. Native gate 504/504 rc 0, 281.2 s wall, 1984 s pooled at load 78-105 (gated at d4d16008, pushed as d8867baf after rebasing over a dots-only commit)."
---

Problem: native-gate-aot-positive ran its two forks through a private START driver inside the sequential group, spending ~28 s alone, and aot-negative sat in the sequential group though it needs only the linker image. Acceptance: aot-positive splits into two ordinary rows (bundle, preseed) at the registry head, the fork driver is deleted, aot-negative moves into the pool beside stripped-address, whitebox rows start right after their hold. Verify: standalone row times; full native gate shows the sequential group without them.
