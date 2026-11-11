---
title: "Linux arm64: arm64-link-run and file-objects fail on spark"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:58:57.259004+02:00"
---

Problem: on spark (Linux arm64) with the Mac-built hb of the cross-OS stack (spark:~/carl-runs/xos4/habu, 2026-11-11), bin/hb --load test/compiler/arm64-link-run.f ends rc 1 with 4 failures, the last 'each file is under 64 KiB: only the kernel code the program reaches is written' (assert 14, expected true got false), and bin/hb --load test/file-objects.f ends rc 1 at 'Q: the build exits 0' (assert 276, expected 0 got 74). Both pass on the Mac. Logs: spark:~/carl-runs/xos4/o/red/arm64-link-run.log, file-objects.log. Cause unknown: neither test has run on Linux before. Acceptance: the cause of each failure is named with path:line and fixed at its owner; both tests pass on spark and on the Mac. Files: unknown until the cause. Verify: both tests on spark. Depends: none. Ownership: carl. Claim: unassigned.
