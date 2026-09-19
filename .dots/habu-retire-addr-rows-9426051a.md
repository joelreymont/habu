---
title: Retire address rows when a REPL line rewinds DATA
status: active
priority: 1
issue-type: task
created-at: "2026-09-20T00:13:38.377424+03:00"
---

Claim: alder, .jj-ws/alder-repl-rows from 84f59696. Hazel released the
evaluate/REPL address-row rollback seam, its seed note and regression tests.

Ready: SNAP-RELOC:LROLLBACK takes the saved DP in x12 and preserves x0..x17;
evaluate and REPL line recovery call this one routine before writing DP.
The stable row filter, backing move and count-based index invalidation are
unchanged. Its clobber contract is explicit; the seed still owns no rows.

The new real-PTY fixture fails six restoration assertions on pristine 84f59696
and passes with the fix: defer, typed quotation and persisted pointer at both
tiers, exact baseline row count/DP restored, then opposite-kind address reuse.
Existing evaluate rollback, including backing movement and unordered rows,
passes. Check-only bootstrap and all 18 focused registry rows pass, including
friend-arena-absence, clobber, tail-pure, hb-build and program-diagnostics.
All three private generations are identical, SHA256
81070f1b4414cc197baf2dc3d4b77ad7a33242c8b1131df81c672fcefd03f9c5.
Astra follow-up review is clear. Artifacts: /tmp/alder-repl-rows.
No local full gate; Hazel chains and closes after integration.

Problem (audit of ec0379f1): src/habu/habu2.f:9171 EM-REPL-RECOVER restores DP from RSAVDP-CELL without retiring the address declaration rows above the cut - the invariant ec0379f1 established for EM-EVAL-THROW-RECOVER. Under a tty (script -q -e -c '... ./bin/hb' /dev/null): 'defer X ( -- n ) NOSUCH-WORD' (E-UNDEFINED, the line is rewound) then 'PERSISTED-PTR-VARIABLE Y' dies 'hb: snapshot address cell kind mismatch rc=99': the defer's code-kind row survives, the pointer declaration reuses the address, the marker's kind check kills the process. Same with 'TYPED-VARIABLE V [ n -- n ]' since 80a9bd5c declares quotation cells at definition. Piped stdin fails closed (rc 70), so only the interactive path is affected. Acceptance: the REPL line recovery runs the same row filter as the evaluate recovery (one shared emitted routine; the REPL frame's saved DP is the cut; same index invalidation and backing move), the forth.fs mirror keeps its no-rows note, and a fixture drives the two lines through the REPL's line path (a pty via script, or the reader directly) and shows the second declaration succeeds with the row count back at the baseline; three generations with gen2 == gen3; check-only bootstrap. Files: src/habu/habu2.f (EM-REPL-RECOVER + the shared routine), bootstrap/cg/forth.fs (comment), test/. Verify: the fixture; test/address-cell-rollback.f; test/run.f. Depends: none. Ownership: engine (alder, cleared for this seam). Claim: unassigned.
