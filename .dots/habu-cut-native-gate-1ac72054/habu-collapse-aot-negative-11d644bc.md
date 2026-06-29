---
title: Collapse AOT-negative hot path
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-06-29T05:31:22.792703+02:00\\\"\""
closed-at: "2026-06-29T06:27:43.092373+02:00"
close-reason: "completed: direct HBB path plus dedicated AOT warm runner cut AOT-negative from the prior 31.334s full hot tail to 14.989s in the hot full gate; focused warm runner proved aot-neg 10.71s and aot-pos 3.39s; full native gate passed hot at 42383ms internal / 45.63s wall after pool-slot RCA fix."
---

Problem: after final hb-build artifact caching, focused AOT-positive hit path is 12.94s and full hot AOT-positive is 15.035s, but full gate remains 43.273s because the critical path moved to AOT-negative (31.334s) plus stdlib check/tool/lint tails. RCA next: inspect test/gate-aot-negative.f and hb-build negative coverage; identify which invariants can run via direct lint/check/source model instead of a full failing maker run, keeping at least one real hb-build CLI/env failure boundary. Acceptance: focused AOT-negative drops materially, full hot gate drops below current 43.273s, all negative diagnostics remain asserted, full native gate green.
