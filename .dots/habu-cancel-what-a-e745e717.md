---
title: Cancel what a short io_uring enter forgets
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:24:57.368802+01:00"
---

Problem: src/lib/aio.f SUBMIT-OR-FORGET forgets a record whose entry a short enter may have left in its ring, without cancelling it, and SCRUB-ONE skips forgotten records; once a later stage enters it, a no-deadline poll or read on a quiet fd runs with no ticket and holds its record and its ring until STOP cancels it (re-review of the ring-growth lane). Acceptance: a forgotten record's operation never runs unwatched: an entry the kernel has not taken is not started, one it took is cancelled. Depends: none. Claim: unassigned.
