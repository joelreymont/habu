---
title: A cast or linear refusal under --load carries a line and no column or bytes
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:01:50.727592+02:00"
---

Problem: src/core/render.f CAST-ERROR PACKET (the known branch, :2454-2456 at 4e1ac4f5) writes "line" alone. Reproducer: a file with 'true DIAG-JSON!' / 'package LIN-FB' / 'public' / 'DEFLINEAR LIN-FB:tok' / 'LINEAR: MINT ( n -- LIN-FB:tok )' / ';package': 'bin/hb --load <file>' gives E-LINEAR-SCOPE with "line":5 and no column, byte_start or byte_end; a required file gives the same. tools/repair-packet.f then ends rc 74 'missing diagnostic field' and the diag-contract check rc 1 'missing JSON field', so the load route yields no repair packet; under tools/check.f the same program carries all four (4/9/51-55) and passes both. Found by the re-review of habu-a-preflight-refusal-87f2d9f2 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rows-rereview2.md, O1), same on the engine before it. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the packet carries all four place fields of the refused token, from the file buffer the load reads, or none; the load and the check agree on the place; repair-packet and the contract accept it. Files: src/core/render.f, test/load-reject-diag-test.f. Verify: the reproducer under --load and under tools/check.f, piped to tools/repair-packet.f. Depends: none. Ownership: CAST-ERROR PACKET. Claim: unassigned.
