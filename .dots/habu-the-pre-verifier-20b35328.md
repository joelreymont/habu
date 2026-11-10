---
title: The pre-verifier stops on a definer whose head leaves an if open
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:01:19.819337+02:00"
---

Problem: the pre-verifier throws 78, E-DUPLICATE-DEFINITION (word 'duplicate-definition', place 1/1/0-20), on a file that defines nothing twice. Reproducer on master and with the wrong-places commit (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rows-review.md, Found outside this commit): ': MK ( -- ) create ; parses: MK 1' / 'MK MADE' / ': MYK ( n -- ) ( c ) MADE drop create dup ,' / '   0= if does> ( -- n ) @ ;'. The same file without the open 'if' passes. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: which pass raises the code and why an open control structure at does> reaches it. Acceptance: the file is refused for the open 'if' at its does>, with that reason and place, by the pre-verifier as by the load. Files: named by the hunt. Verify: the reproducer in test/diag-position-test.f. Depends: none. Ownership: the pre-verifier's does> head. Claim: agent=bob workspace=.jj-ws.noindex/habu-the-pre-verifier-20b35328

Hunt 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/openif-hunt.md): the pre-verify checks the head twice in an unknown tick order; the first, parent-order pass defers and records MYK (checker.f CHECK, the deferred record branch), the second meets that record and CHECK-REC-ADMIT throws a bare 78. Attribution inferred, not traced. Fix in work (briefs8/Record-a-deferred-definer-head-once.md): the deferred record branch records nothing in a parent-order pass. Three other defects it found are habu-an-empty-does-*, habu-the-check-refuses-* and habu-two-renderers-place-* (see dot ls).

Fix 2026-11-10: vunmuxwl a11646b1 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/openif-fix.md), review in work (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Review-deferred-head-once.md). The base file's refusal is placed in the comment-stripped copy of the definition (3/45 where the load means 4/10); the review says whether habu-a-preflight-refusal-87f2d9f2 covers it.
