---
title: A top-level stack underdepth writes no JSON record
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T13:57:06.562112+02:00"
---

Problem: under 'bin/hb --load tools/check.f -- --json-errors', a top-level 'drop', '+' or '1 2 drop drop drop' on an empty stack is refused by the engine's gate with prose only, 'hb: interpret stack underdepth: drop' (src/habu/outer.f:601 GATE, src/habu/habu2.f:175 MINMSG$), and no record is written, so tools/gate-json-assert.f diag-contract fails with 'no JSON diagnostics'. Found by the hunt for habu-a-top-level-adb73a06 (report /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/toprec-hunt.md, cases /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/toprec). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the refusal writes one record in the top-step shape habu-a-top-level-adb73a06 settles, with the token and its place. Files: src/habu/outer.f, src/core/render.f, tools/check-core.f. Verify: the three programs through check.f and diag-contract. Depends: habu-a-top-level-adb73a06 (the record shape). Ownership: the checker's diagnostics. Claim: unassigned.

Acceptance case 2026-11-10 (bob; merged from habu-a-refused-top-b43e6cea "A refused top-level step writes no packet under --all-errors", one cause with this dot): Problem: under tools/check.f --json-errors --all-errors a top-level '1 UFB' that is refused gets no packet (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/pin-review/p8/s2.f and p3/all.f), on master and on habu-a-refusal-s-4e54728f's engine alike. Found by that lane's review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/pin-rows-review.md, outside the brief). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: every refusal --all-errors meets writes its packet, a top-level step's in the shape habu-a-top-level-adb73a06 settles; a case pins it. Files: tools/check-verify-core.f, src/core/render.f, test/diag-position-test.f. Verify: the two programs. Depends: habu-a-top-level-adb73a06. Ownership: the checker's diagnostics.
