---
title: Case arms that leave different depths are refused without a reason
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T13:31:22.403124+02:00"
---

Problem: ': X ( n n -- n ) case 1 of 5 5 endof 2 of 5 endof endcase ;' prints "at 'endof' expected: n n actual: n n n" and no reason, where an if whose arms differ now says "the if and else arms leave different stack depths" (checker.f MD-ARMS-DEPTH, reason 62). Found by the review of habu-no-test-pins-f2d39cfb. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a case arm that leaves another depth than the arms before it is refused with a reason that says so, in prose and in the packet's reason field; a test in test/diag-position-test.f pins token, rows and reason. Files: src/core/checker.f (the endof and endcase joins, the MD reason constants), src/core/render.f (the reason text), test/diag-position-test.f. Verify: the program above on bin/hb; the test fails on master's engine. Depends: habu-no-test-pins-f2d39cfb landed. Ownership: the checker's control-flow reasons. Claim: unassigned.
