---
title: "check.f --json-errors misplaces or omits the record for a TRUSTED: definition"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T13:31:22.429892+02:00"
---

Problem: 'bin/hb --load tools/check.f -- --json-errors' on a TRUSTED: definer whose does> clause leaves an if open (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/ncomp/b/t9.f) writes a packet at line 1 column 18 though the ';' is at line 4 column 3; on 'TRUSTED: TM7 ( -- n )' / '0 if 1 ;' (t7.f) it writes only the ncomp prose line and exits 70 with no JSON record. Both on the engine before and after habu-an-open-if-7b86a774. Found by that lane's review. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names the cause of each as path:line (one cause or two); then both write one record at the refused token's own line and column, and a test pins each. Files: tools/check.f and the words it calls for a TRUSTED: body. Verify: the two files; the tests fail on the engine before. Depends: none. Ownership: tools/check.f. Claim: unassigned.
