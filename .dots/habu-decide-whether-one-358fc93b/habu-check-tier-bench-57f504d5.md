---
title: Check tier-bench results outside the timing
status: open
priority: 1
issue-type: task
created-at: "2026-10-26T11:28:18.206816+02:00"
---

tools/tier-bench.f writes SINK but never reads it (tier-bench.f:126,186-220): a miscompiled kernel times as fast. Assert each kernel's result after the timed region so measurements of the tier 0 steps are trustworthy. Experiment order (Joel 2026-10-26): fixes first.
