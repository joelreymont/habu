---
title: "Measure arm64's status for a foreign fault"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:28:15.899298+01:00"
---

Problem: an x86-64 engine exits 134 (CRASH-RC) for a fault outside Habu code; what an arm64 engine exits with for the same fault is not traced, so habu-name-each-exit-3fc49d37's table may name one target's status only. Acceptance: arm64's status for a fault outside Habu code (a C callback that dereferences 0) is measured on Linux and macOS, and the exit-status table names one status both targets use. Depends: none. Claim: unassigned.
