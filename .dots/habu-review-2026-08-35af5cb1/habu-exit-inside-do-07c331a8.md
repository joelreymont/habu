---
title: exit inside do-loop without unloop certifies
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.822102+02:00"
---

Problem: src/core/checker.f:10169-10175 CF-EXIT consults no DO frame and CF-UNLOOP is empty, so ': W ( -- ) 3 0 do exit loop ;' certifies; the engine's J-EXIT (habu2.f:2533-2540) drops the locals frame only, so each call leaks one loop-stack frame (LOOPSP restored only by throw recovery 8127-8131): an enclosing loop's i/j read the wrong frame and repeated calls walk toward LOOP-STK-FRAMES. docs/forth.md:864-868 promises path-sensitive EXIT. Also CF-I/CF-J (10224-10232) scan across the quotation boundary CF-FINDDO stops at, so '[: i ;]' inside a loop certifies. Acceptance: Checker-Miss RCA; CF-UNLOOP discharges one DO frame and CF-EXIT rejects with an undischarged DO frame between it and the nearest quotation boundary; i/j refused across a quotation boundary; negative fixtures for both; the tree's own 'i unloop exit' idiom stays green. Files: src/core/checker.f, test/checker-*.f. Verify: the fixtures red-then-green under bin/hb --load tools/check.f. Depends: none. Ownership: checker control flow. Claim: unassigned.
