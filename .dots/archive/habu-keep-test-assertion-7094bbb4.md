---
title: Keep test assertion counts across Tasks
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T08:14:45.760755+03:00"
closed-at: "2026-10-16T09:56:18.163626+03:00"
close-reason: Atomic case and failure increments preserve exact concurrent counts. Pre-code regression failed; focused assertion harness now passes with 400000 of 400000 cases. Astra review and ordering follow-up clear.
---

On public 600eb464-4849828a two Tasks each executing 200000 T-ASSERT calls report 382164, 206873 or 207281 cases instead of 400000. src/lib/test/assert.f T-NEXT and T-FAIL+ use shared @ 1+ !. Use atomic increments at the shared-counter boundary and state the concurrency contract; preserve ordinary assertion/refusal behavior. Pre-code reproducer: /private/tmp/claude-501/-Users-joel-Work-kaizen/83ad8c8d-b99e-4097-89eb-3057028876e9/scratchpad/race/race.f. Verify exact joined totals and existing harness E2Es, with independent review.
