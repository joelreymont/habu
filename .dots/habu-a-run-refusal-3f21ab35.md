---
title: A run refusal in a file the subject loads by path names the subject file
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:13:08.204104+02:00"
---

Problem: t.f = 's" <abs>/q.f" required' with q.f = ': MK ( -- ) create ; parses: MK 1' / 'MK MADE' / ': P ( n -- ) ( c ) MADE drop' / '   if ;'. The load says q.f:4; 'tools/check.f -- --json-errors t.f' answers E-REJECTED ';' in t.f at 2/7 bytes 33-34 (1/25 24-25 before habu-a-preflight-refusal-87f2d9f2). Through '--source-list base.f base.f' the file is named '<source-list>' at 1/43 42-47. Run by the review of habu-the-pre-verifier-20b35328 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/openif-review.md O1), same on master; not rerun by the lead. Cause as the review read it: src/core/render.f DFILE$ names DIAGFB, the run's label tools/check-core.f CHK-BUILD-PREFIX sets once for the whole run, for every definition packet, where TOP-FILE$ uses the label only while the load reads that file; and the origin DIAGL0/C0/B0 comes only from the subject's inline markers, so ROWS-ARM (src/core/check-hook.f) counts a dependency's rows from the subject's origin. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a refusal in a file reached by required, included or a source list names that file and the token's line, column and bytes in it, the same place the load names. Files: src/core/render.f, tools/check-core.f, src/core/check-hook.f, test/diag-position-test.f. Verify: the reproducer in test/diag-position-test.f, failing before. Depends: none. Ownership: the file a definition packet names. Claim: unassigned.
