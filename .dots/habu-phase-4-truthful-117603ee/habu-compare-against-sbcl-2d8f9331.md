---
title: Compare against SBCL canonically
status: open
priority: 2
issue-type: task
created-at: "2026-04-01T22:06:02.395029+02:00"
blocks:
  - habu-upgrade-gc-from-fcd88477
---

Problem: Habu versus SBCL comparison is meaningless unless workloads and methodology align. Acceptance: apples-to-apples per-workload, geomean, memory, and GC comparisons are reproducible on the canonical workload set. Files: Habu and SBCL bench harnesses and reports. Verify: reproducible comparison report with explicit methodology and provenance. Blockers: habu-upgrade-gc-from-fcd88477; also depends on habu-optimize-interpreter-hot-365f8061 and habu-expand-jit-from-033b2237.
