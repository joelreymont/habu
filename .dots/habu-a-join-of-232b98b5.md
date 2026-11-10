---
title: A join of n with i64 inside a type parameter or a quotation row is accepted
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T13:57:06.573656+02:00"
---

Problem: where two control-flow paths meet, a cell that is jbox<n> on one path and jbox<i64> on the other, or a quotation whose row holds n on one path and i64 on the other, is accepted in one arm order (the n arm first) and refused in the other, with and without the fix of habu-the-checker-accepts-fbca7120, which joins only top-level cells (ROW-JOIN-PREP, src/core/checker.f); CON-OK? passes nested pairs. Programs: /Users/joel/Work/habu/.jj-ws.noindex/bob-join/.out/jf/ (copied to /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/join when the lane retires). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: both programs are refused in either arm order, each case accepted by the engine before; nothing in the tree relied on the accept (the engine builds, the suite passes). Files: src/core/checker.f, test/diag-position-test.f. Verify: the two programs and their mirrors; the suite. Depends: habu-the-checker-accepts-fbca7120 landed (same words). Ownership: the checker's joins. Claim: unassigned.
