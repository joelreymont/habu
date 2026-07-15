---
title: Migrate remaining test allocation callers
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-15T12:16:45.231504+02:00\""
---

Census gap found by the testalloc lane 2026-07-15: two MEM-ALLOC-BYTES callers landed after the six-file enumeration (tfam's owner-persist wave): test/owner-wid-doctor.f:72 (size from FILE-SIZE -> IMG-A) and test/engine-error-package.f:92 (u from FILE-SIZE -> IMAGE-A). Both read image files; a zero-size image is corrupt input, so MEM:BYTES-ALLOC-LEN's E-MEM-SIZE throw is the correct fail-closed behavior (no zero-legal path). Fix: the one-line typed pattern in both; their owning suites green. Ownership: test harness allocation.

Claim: agent=fable-main workspace=default (integrator micro-fix)
