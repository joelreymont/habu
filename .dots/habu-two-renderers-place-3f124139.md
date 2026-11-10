---
title: Two renderers place a nameless duplicate stop differently
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T18:18:51.057509+02:00"
---

Problem: a pre-verify stop with code 78 and no recorded name (a bare throw: checker.f CLAUSE, EXPORT-ROOM, USIG-CERT-ADMIT, CHECK-REC-ADMIT) is rendered by src/lib/verify-diagnostics.f :86-100 at the stop's token and by tools/check-all-errors-core.f :270-276 (CA-DUP-RECORD$) as token 'duplicate-definition' at 1/1 bytes 0-20. Read by the hunt of habu-the-pre-verifier-20b35328 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/openif-hunt.md, item 4 and Other defects); the one known reproducer is that dot's file, which its fix removes. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first, on the tree with that fix: a program that still reaches a bare 78 (a clause name already held; an export whose clause name is held), what each renderer prints for it, and which place is true. Acceptance: both renderers give one packet for a duplicate stop, with a token the file holds and its place. Files: src/lib/verify-diagnostics.f, tools/check-all-errors-core.f, as the hunt confirms. Verify: the hunt's reproducer in test/diag-position-test.f. Depends: habu-the-pre-verifier-20b35328. Ownership: the duplicate stop's packet. Claim: unassigned.
