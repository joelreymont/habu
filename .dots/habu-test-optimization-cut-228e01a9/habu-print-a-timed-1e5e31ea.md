---
title: "Print a timed-out entry's capture in GE-EXPECT rc checks"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T11:56:01.703348+02:00\""
closed-at: "2026-10-01T17:16:44.020259+02:00"
close-reason: Fixed by efde7ead (review 165 ACCEPT)
---

Problem: test/gate-common-lib.f GE-EXPECT-OK, GE-EXPECT-RC and GE-EXPECT-NONZERO (~:228-235 at r4-wblabel 643f4cab) evaluate GT-RC@ before GE-FAIL; on the entry's own deadline GT-RC@ throws E-PROC-TIMEOUT first, so GE-FAIL's print-then-throw (label, outcome, stats, argv, stdout, stderr, then the throw; docs/gate.md) never runs for the rc checks and a persistent hang leaves only 'hb: uncaught throw code -2502'. Evidence: $HOME/.cache/tmp/kestrel-r4-wblabel/log/post-ge.log vs pre-ge.log (outer-interpret, forced 1 ms). Found by review 122. Acceptance: a timed-out entry under each of the three words prints GE-FAIL's capture and then throws E-PROC-TIMEOUT (pool label TIMEOUT-UNDER-LOAD unchanged); a case forcing a 1 ms deadline is seen failing first. Files: test/gate-common-lib.f and its test.
