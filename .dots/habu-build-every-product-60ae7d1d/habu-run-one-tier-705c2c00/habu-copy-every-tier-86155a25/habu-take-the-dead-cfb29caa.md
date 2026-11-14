---
title: Take the dead flag through a ticked callee
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T19:01:45.620198+03:00"
---

Problem: the checker's tick route, src/core/checker.f:20910 BTICK-EDGE, takes a ticked callee's throw edge and intact masks but not its CTL-DEAD flag (a call with no normal continuation, :5508). So `['] WDEAD catch` is fit-checked against WDEAD's declared output row instead of the stale rule (docs/forth.md:1505-1511); test/catch-stale-suite.f T11 (:512) pins that. The cause is tier 1, not the checker: with the flag, a dead body publishes CWIN-OUT = CELLS-NONE, and tier 1's elaborator (DO-CATCH and DO-FINALLY, deleted with tier 1 in c7dec3ddaf05) makes that the body's calling convention, which its QFILL refuses for a tick whose row carries an existing routine's ABI (E-NELAB-QUOT -8651, "ncomp: cannot compile THROWING at [']"). Tier 1 changes only to fix a defect on a path it still owns (docs/design.md:44-46), so the fix waits for tier 1 to go and is checker work only. This is an acceptance case of its parent: fix it in the code that stays, never in the machinery the parent replaces.

Acceptance: once tier 1 is deleted, BTICK-EDGE takes CTL-DEAD; T11 flips to the stale-rule refusal (E-STALE-READ); `( ptr u8 -- n ) ['] WBOOM catch {: v code:n :} code` and `['] W ['] CLEAN finally` with a never-returning W compile and run under T0; the comments at checker.f:21921-21923 and catch-stale-suite.f:509-511 lose the tier-1 exception.

Depends: none.

Claim: unassigned.
