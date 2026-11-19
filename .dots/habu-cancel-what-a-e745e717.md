---
title: Cancel what a short io_uring enter forgets
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:24:57.368802+01:00"
---

Problem: src/lib/aio.f SUBMIT-OR-FORGET (:649-651 on master) forgets a record whose entry a short enter may have left in its ring, without cancelling it, and SCRUB-ONE skips forgotten records; once a later stage enters it, a no-deadline poll or read on a quiet fd runs with no ticket and never completes, so its record never frees, its ring never closes and STOP answers E-AIO-BUSY from then on (re-review of the ring-growth lane). Acceptance: a forgotten record's operation never runs unwatched: an entry the kernel has not taken is not started, one it took is cancelled; a test where a forgotten no-deadline poll on a quiet fd lets STOP succeed. Depends: none. Claim: unassigned.
