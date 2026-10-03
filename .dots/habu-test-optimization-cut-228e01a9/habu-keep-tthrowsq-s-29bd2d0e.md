---
title: "Keep TTHROWSQ's expected code per call"
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T13:14:26.839419+02:00"
---

Problem: lib/test/assert.f:136-139 (at r4-wb-trunc c2685a57) TTHROWSQ keeps the expected throw code in the global T-EXPECTED#, so an xt under test that runs its own TTHROWSQ overwrites the outer check's expected code and the outer check compares against the inner one. Found by the r4-wb-trunc worker (dot b2e8c903); no nested use found in the tree yet. Acceptance: the expected code lives with its call (on the return stack or a local, not a shared global), a nested TTHROWSQ inside the xt of an outer TTHROWSQ is checked against its own code and the outer against its own, through the real lib/test/assert.f load path, seen failing first; every existing TTHROWSQ caller still passes. Base: r4-wb-trunc c2685a57 (it changes TTHROWSQ). Files: lib/test/assert.f and its test.
