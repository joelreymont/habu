---
title: Maxima fast execution plan
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:04.152094+01:00"
---

PLAN.md:1 define end-to-end plan so Habu runs upstream Maxima generically and fast. Root cause: compatibility and perf workstreams are interleaved without hard stage gates. Fix: staged dot tree with correctness gates, perf gates, and hoist API drift checks.
