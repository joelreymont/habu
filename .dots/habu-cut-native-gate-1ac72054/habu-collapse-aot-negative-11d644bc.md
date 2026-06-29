---
title: Collapse AOT-negative hot path
status: open
priority: 1
issue-type: task
created-at: "2026-06-29T05:31:22.792703+02:00"
---

Problem: after final hb-build artifact caching, focused AOT-positive hit path is 12.94s and full hot AOT-positive is 15.035s, but full gate remains 43.273s because the critical path moved to AOT-negative (31.334s) plus stdlib check/tool/lint tails. RCA next: inspect test/gate-aot-negative.f and hb-build negative coverage; identify which invariants can run via direct lint/check/source model instead of a full failing maker run, keeping at least one real hb-build CLI/env failure boundary. Acceptance: focused AOT-negative drops materially, full hot gate drops below current 43.273s, all negative diagnostics remain asserted, full native gate green.
