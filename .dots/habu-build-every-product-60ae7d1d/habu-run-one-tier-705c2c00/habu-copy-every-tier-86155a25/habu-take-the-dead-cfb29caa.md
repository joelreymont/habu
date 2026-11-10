---
title: Take the dead flag through a ticked callee
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T19:01:45.620198+03:00"
---

Problem: the checker's tick route, src/core/checker.f:24237 BTICK-EDGE, takes a ticked callee's throw edge and intact masks but not its CTL-DEAD flag (a call with no normal continuation, :5285). So `['] WDEAD catch` is fit-checked against WDEAD's declared output row instead of the stale rule (docs/forth.md:1523-1529); test/catch-stale-suite.f T11 (:466-472) pins that. The cause is tier 1, not the checker: with the flag, a dead body publishes CWIN-OUT = CELLS-NONE, and tier 1's elaborator (src/compiler/native/elaborate.f DO-CATCH :3490, DO-FINALLY :3510) makes that the body's calling convention, which its QFILL refuses for a tick whose row carries an existing routine's ABI (E-NELAB-QUOT -8651, "ncomp: cannot compile THROWING at [']"; the comment at checker.f:24228-24236 records the measurement). Tier 1 changes only to fix a defect on a path it still owns (AGENTS.md:63-64), so the fix waits for tier 1 to go and is checker work only. This is an acceptance case of its parent: fix it in the code that stays, never in the machinery the parent replaces.

Acceptance: once tier 1 is deleted, BTICK-EDGE takes CTL-DEAD; T11 flips to the stale-rule refusal (E-STALE-READ); `( ptr u8 -- n ) ['] WBOOM catch {: v code:n :} code` and `['] W ['] CLEAN finally` with a never-returning W compile and run under T0; the comments at checker.f:24228-24236 and catch-stale-suite.f:466-469 lose the tier-1 exception.

Files: src/core/checker.f, test/catch-stale-suite.f.

Verify: bin/hb --load test/catch-stale-suite.f, and a two-line program with each form above run under bin/hb.

Depends: habu-delete-tier-1-f5c44fb4.

Ownership: unassigned. Claim: unassigned.
