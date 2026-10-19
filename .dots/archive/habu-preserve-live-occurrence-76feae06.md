---
title: Correct occurrence helper clobber contracts
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T19:08:34.928426+03:00"
closed-at: "2026-10-19T19:29:56.850745+03:00"
close-reason: "Added x15 to the occurrence helpers shared preserve mask and reconciled the comment with master: LCOUNT leaves x15 untouched, while APPEND and RESET inherit that preservation. Source lint is clean at 442 routines/762 calls; existing negative fixtures pass with expected findings. The independent preservation review is clear. Only lint metadata changed, so no engine rebuild or runtime qualification is required."
---

The definition occurrence helpers' lint preserve mask omits x15. On master, LCOUNT leaves x15 untouched; LAPPEND and LRESET inherit its preservation. Add x15 to their existing shared mask and explain the contract without changing emitted behavior or ABI. The private development tree reproduced three false findings when LCOUNT used and restored x15; this metadata correction is independent of those emitter changes.
