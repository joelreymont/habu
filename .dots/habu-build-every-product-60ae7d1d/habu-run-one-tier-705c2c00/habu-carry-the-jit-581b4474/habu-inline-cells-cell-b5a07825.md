---
title: Inline cells, cell+ and ptr-field in T0
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T09:56:38.297820+02:00"
closed-at: "2026-11-04T11:48:47.849899+02:00"
close-reason: "Census 2026-11-04, done: Landed 39e74f25 \"Compute cells, cell+ and ptr-field in T0\"; jerry-t0c1 not listed."
---

JIT emits one lsl or add; T0 calls. Front-end rows plus seam shift/add-immediate use; fold under constants as the JIT does.

rsnyxvvn (jerry-t0c1): cells as lsl by CELL-SHIFT, cell+ as 8 +, ptr-field as the shifted index plus the base (habu1.f BPTRFIELD), folding constants; under review.
