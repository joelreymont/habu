---
title: The pre-verifier stops on a definer whose head leaves an if open
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:01:19.819337+02:00"
---

Problem: the pre-verifier throws 78, E-DUPLICATE-DEFINITION (word 'duplicate-definition', place 1/1/0-20), on a file that defines nothing twice. Reproducer on master and with the wrong-places commit (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rows-review.md, Found outside this commit): ': MK ( -- ) create ; parses: MK 1' / 'MK MADE' / ': MYK ( n -- ) ( c ) MADE drop create dup ,' / '   0= if does> ( -- n ) @ ;'. The same file without the open 'if' passes. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: which pass raises the code and why an open control structure at does> reaches it. Acceptance: the file is refused for the open 'if' at its does>, with that reason and place, by the pre-verifier as by the load. Files: named by the hunt. Verify: the reproducer in test/diag-position-test.f. Depends: none. Ownership: the pre-verifier's does> head. Claim: agent=bob workspace=.jj-ws.noindex/habu-the-pre-verifier-20b35328
