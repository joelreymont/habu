---
title: An empty does> clause body is refused at line 1 with no token
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:18:51.034806+02:00"
---

Problem: ': MYK ( n -- ) drop does> ( -- ) ;' is refused E-MISMATCH, remove_producer, word myk;does, with token "" at 1/1 bytes 0-0 under --load tools/check.f -- --json-errors, and the load says "in myk;does: at '' expected: actual: ptr a". Reproduced by the lead on master 951b3f90. Found by the hunt of habu-the-pre-verifier-20b35328 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/openif-hunt.md, Other defects). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: where a clause's end-of-body mismatch takes its token and place, and why an empty body has none. Acceptance: the packet and the load's message name the clause's closing token (or the does> when nothing follows it) with its line, column and bytes. Files: named by the hunt. Verify: the reproducer and a two-line variant in test/diag-position-test.f. Depends: none. Ownership: the place of a clause body's closing mismatch. Claim: agent=bob workspace=.jj-ws.noindex/habu-an-empty-does-8710ad84

Hunt 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/emptyclause-hunt.md): a clause's end-of-body mismatch pins no token of its own, and an empty clause walks none. Settled: the packet and the load name the closing ';'. Fix quymnqyy fce59ec0 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/emptyclause-fix.md), review in work (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Review-empty-clause-closer.md).
