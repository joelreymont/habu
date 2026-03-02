---
title: Optimize interpreter hot paths
status: open
priority: 2
issue-type: task
created-at: "2026-04-01T22:06:02.376498+02:00"
blocks:
  - habu-define-canonical-workload-ae1f969c
---

Problem: interpreter overhead on clean workloads remains high. Acceptance: measured call, dispatch, allocation, and stream-path hot spots improve with regression coverage and no semantic cheats. Files: hot-path runtime files proven by profiling. Verify: clean workload A/B profiles and regressions. Blockers: habu-define-canonical-workload-ae1f969c.
