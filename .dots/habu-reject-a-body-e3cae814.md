---
title: "Reject a body's tick of a top-only primitive"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T12:45:20.187069+01:00"
---

Problem: a body's call of a primitive whose rows are all top-only is rejected (src/core/check-walk.f TRY-PRIMS asks PE-TAKES?), but its tick is admitted as an opaque xt: neither BTICK-TOK nor TICK-REJECTED? (src/core/checker.f) asks PE-TAKES?. Found by reading; no failing program is reduced yet. Acceptance: a body's `[']` of such a primitive is rejected where its call is, with the call's record, by a test that fails before the fix. Depends: none. Claim: unassigned.
