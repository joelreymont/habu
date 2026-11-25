---
title: "Judge a left quotation's call after refusals"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:35:26.406993+02:00"
---

Problem: tools/check.f --verify-only passes (W-CHECK-DEFERRED, rc 0) bodies the load refuses (rc 70) when a local-reading quotation meets an NS-LEFT block: DW-QUOT enters LEFT-WHY, which CHECK-VERDICT ranks above every refusal and REFUSAL-STEP treats as ending judgment (src/core/checker.f CHECK-VERDICT, REFUSAL-STEP, BLOCK-PAIR's NS-LEFT arm fails the unify); a later mismatch, a mismatch at the same call and a reading quotation at RECURSE all pass, and a later stop names the quotation as its cause. Reproducers: four programs, each with s" : GEN ( -- ) ;" evaluate and : RUN ( [ -- ] -- ) GEN execute ; then (1) : PUT ( n -- ) {: s :} [: s drop ;] RUN s" x" 1 + drop ; (2) RUN as ( n [ -- ] -- ) GEN execute drop and : PUT ( n -- ) {: s :} s" x" [: s drop ;] RUN ; (3) : REC ( [ -- ] n -- ) {: q n :} n 0 > IF q RUN [: n drop ;] n 1 - RECURSE THEN ; (4) : IM ( -- ) ; immediate and : PUT ( n -- ) {: s :} [: s drop ;] RUN IM ['] ffi-call drop ;. Acceptance: (1)-(3) are refused by --verify-only with the load's code (E-MISMATCH at + , E-MISMATCH at RUN, E-LOCAL-ESCAPE at RECURSE); (4)'s W-CHECK-STOPPED names the tick, not a quotation; TEST-QUOT-LEFT and TEST-QUOT-LEFT-FACT still pass. Depends: none. Claim: unassigned.
