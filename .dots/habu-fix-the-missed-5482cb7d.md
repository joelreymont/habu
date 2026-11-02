---
title: Fix the missed TMPDIR-MKDIR caller in take-test.f
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T18:22:35.301585+02:00"
---

Problem: test/browser/take-test.f:41 calls TMPDIR-MKDIR, which no file defines; the FS:MKTEMP-DIR rename (26ac40c2) missed it (tim's reading), so the file cannot load. Acceptance: the call uses FS:MKTEMP-DIR with its current effect, and the file loads. Files: test/browser/take-test.f. Verify: load the file the way its runner does (it may need --chrome; Chrome runs only when asked, LESSONS 2026-10-31). Depends: none. Ownership: unassigned. Claim: unassigned.
