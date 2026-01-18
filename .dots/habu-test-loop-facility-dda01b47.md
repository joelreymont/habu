---
title: Test LOOP facility comprehensively
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T06:24:50.577052+02:00"
---

Files: test/ (new test file)
LOOP is claimed ✓ at stdlib.habu:3730.
Create comprehensive test covering:
- Simple iteration
- Multiple variables
- Conditionals
- Collection (append, nconc)
- Early termination (loop-finish)
Verify: zig build test passes with new tests.
Est: 30min
