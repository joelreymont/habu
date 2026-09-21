---
title: Pin ACAP-NORMALIZE-DSITES with a unit fixture
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T13:52:58.161970+03:00"
---

Problem (audit of 4a5c20c8): the window-relative capture coordinate (stored = v - d0 + (d0 and 7), restored = stored + delta with delta a multiple of 8) is covered only by test/aot-artifact-rows.f SAVE-DSITES/ROW-TEST-DATA-VALUE, which re-derives the formula from live memory against the captured blob; test/aot-data-sites.f exercises ACAP-ADD-DSITE/ACAP-ADD-CSITE bookkeeping only. There is no fixture that feeds ACAP-NORMALIZE-DSITES known site values and pins the rewritten ones, including a d0 with each residue 0..7 and a held (ACAP-DSITE-HELD?) duplicate that must not shift twice. Acceptance: such a fixture in test/aot-data-sites.f with the arithmetic stated in its labels; the seed's SUB/ADD residue step (habu2.f:5516-5519) referenced from the fixture comment. Files: test/aot-data-sites.f. Verify: the suite; test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
