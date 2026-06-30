---
title: Eliminate remaining helper and inner hb launches
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-30T23:24:33.517886+02:00\""
---

Problem: Mac hot test suite still reports helper-spawn around 69, inner-hb around 13, inner-hb-stdin around 5, boundary around 18. This keeps suite-body time high even after parent-load removal. Fix: inventory every helper spawn with test name and subject, replace non-boundary launches with resident APIs, batch exact candidate launches by subject, and keep only explicit process/PTY/CLI boundary tests. Acceptance: helper-spawn <=25, inner-hb + inner-hb-stdin <=8, boundary <=12 on Mac hot profile, with docs/gate.md updated if target changes.

Progress: macos-arm64-12x2 hot proof on 2026-06-30 reports helper-spawn=54,
inner-hb=11, inner-hb-stdin=5, and boundary=16. Keep open: all counters remain
above the target even though the suite body is now under the hot wall target.
