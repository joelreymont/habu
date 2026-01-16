---
title: Test GC performance improvements
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:18:51.011169+02:00"
---

Files: tests/
Add tests:
- Verify zero allocations during GC (detector)
- Large heap stress (100MB+)
- Adaptive sizing (queues grow)
- Regression: pause time ≤ baseline
Compare to /tmp/gc-baseline.txt.
Dependencies: habu-add-gc-performance-21db87fa
Verification: all tests pass, no regression
