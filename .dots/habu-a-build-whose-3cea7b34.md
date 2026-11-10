---
title: "A build whose top level ends the process skips the builder's exit actions"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:07:44.371212+02:00"
---

Problem: a program whose top level calls AT-EXIT:ADD and then ends the process (`3 die`) during hb-build exits with the program's status: the program's action runs, hb-build's own exit actions do not (its line 'ended the process during the build' is not printed, its temporary directory is left). Without the ADD the builder exits 74 with the line. Cause: the build space's copy of AT-EXIT replaces the builder's process-exit vector (at-exit.f:88-90; DECISIONS 2026-11-09 'a build that ends the process': 'AT-EXIT owns the vector, so ADD replaces whatever a raw store put there'), and two copies of the owner then live in one process. Found by the builder-crash hunt (/Users/joel/Work/reviews/habu-10x-2026-11-10/hunt-builder-segv.md, case x-die). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Open, to settle from the landed code: what a build-time registration means. Ways seen: the copy's runner calls the vector it replaced after its own actions (the builder's still run; needs a rule against two copies chaining to each other); the build's top level registers with the builder's AT-EXIT; AT-EXIT:ADD refuses while a build is open. Acceptance: the case exits 74 with hb-build's line and leaves no temporary directory; a state table of the vector for one and two copies is beside the code. Files: src/lib/at-exit.f, src/habu/linker.f, tools/hb-build-lib.f. Verify: a case in test/exit-hook-test.f that fails on the base. Depends: habu-the-builder-dies-5e13e362 and habu-a-process-cell-9ddedca3 landed (same words). Ownership: AT-EXIT. Claim: unassigned.

Case 2026-11-10 (bob), from the review of habu-a-process-cell-9ddedca3: a file the build's top level writes and registers with FS:CLEANUP+ is removed by neither the builder nor the product (/Users/joel/Work/reviews/habu-10x-2026-11-10/cells-review/probes/p1.f: its nine build-time files remain after the build and after the product runs). What a build-time registration means settles this case too.
