---
title: Add error check build step
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:15:25.529469+02:00"
---

Files: build.zig
Add check-errors step:
- const check_err = b.step("check-errors", "Check for error masking");
- Run rg commands for forbidden patterns
- Fail build if any found (except // unreachable: comments)
- Make test step depend on check-errors
Dependencies: habu-fix-err-masking-619ef7fa
Verification: zig build check-errors passes, adding forbidden pattern fails build
