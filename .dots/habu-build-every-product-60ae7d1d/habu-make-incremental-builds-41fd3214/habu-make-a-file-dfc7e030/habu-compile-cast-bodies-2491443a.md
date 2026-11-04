---
title: Compile cast bodies through DEF-FIXED
status: closed
priority: 2
issue-type: task
created-at: "2026-11-04T14:14:14.883991+02:00"
closed-at: "2026-11-04T19:43:38.283696+02:00"
close-reason: "landed: f7e597db 22ce649d ca0cb629 715f92ad 92b7dbcc on 3378e2d6. Proof at the tip candidate (/tmp/jerry-step1-land/hb, built by the installed hb, rc 0): compiler-shadow row (shadow.f then shadow-kinds.f) rc 0, 29 records ok, 0 stale; full suite ran 521 of 521, one red: pg (pg-kill-test REPORT-READ READY-MS 60 s expired under load, rc 67 -2502, the known case in habu-remove-the-test-6fc078db), which passes alone twice on the same candidate (5.3 s). Reviews: kinds-a, kinds-b, evalcell and the tip ACCEPT after folds."
---

Step 1 lane kinds-b. definers.f DEF-CAST-ROW and DEF-LINEAR end in the kernel's def-cast, whose shadow is tier 1's SHADOW-FIXED (no unplaced arm64 emission): shadow-kinds measured -6616 for cast: and linear:. They take 0 COMP-BODY:cast DEF-FIXED DEF-CLOSE on every machine; T0's FIXED-BODY gets the identity arm and tier 1's COMPILE-FIXED stops refusing cast (the engine build runs at latch 1; NELAB:FIXED already builds FUN-CAST). Workspace .jj-ws.noindex/jerry-kinds-b on vslnprlw (kinds-a); brief /tmp/jerry-big/kinds-b-brief.md.
