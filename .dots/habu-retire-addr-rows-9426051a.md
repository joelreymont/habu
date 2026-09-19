---
title: Retire address rows when a REPL line rewinds DATA
status: open
priority: 1
issue-type: task
created-at: "2026-09-20T00:13:38.377424+03:00"
---

Problem (audit of ec0379f1): src/habu/habu2.f:9171 EM-REPL-RECOVER restores DP from RSAVDP-CELL without retiring the address declaration rows above the cut - the invariant ec0379f1 established for EM-EVAL-THROW-RECOVER. Under a tty (script -q -e -c '... ./bin/hb' /dev/null): 'defer X ( -- n ) NOSUCH-WORD' (E-UNDEFINED, the line is rewound) then 'PERSISTED-PTR-VARIABLE Y' dies 'hb: snapshot address cell kind mismatch rc=99': the defer's code-kind row survives, the pointer declaration reuses the address, the marker's kind check kills the process. Same with 'TYPED-VARIABLE V [ n -- n ]' since 80a9bd5c declares quotation cells at definition. Piped stdin fails closed (rc 70), so only the interactive path is affected. Acceptance: the REPL line recovery runs the same row filter as the evaluate recovery (one shared emitted routine; the REPL frame's saved DP is the cut; same index invalidation and backing move), the forth.fs mirror keeps its no-rows note, and a fixture drives the two lines through the REPL's line path (a pty via script, or the reader directly) and shows the second declaration succeeds with the row count back at the baseline; three generations with gen2 == gen3; check-only bootstrap. Files: src/habu/habu2.f (EM-REPL-RECOVER + the shared routine), bootstrap/cg/forth.fs (comment), test/. Verify: the fixture; test/address-cell-rollback.f; test/run.f. Depends: none. Ownership: engine (alder, cleared for this seam). Claim: unassigned.
