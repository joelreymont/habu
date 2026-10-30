---
title: Count only refusals a multi-error load reports
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T23:33:22.905972+02:00"
---

Problem: inside MULTI-ERR-BEGIN/MULTI-ERR-END, evaluating 'TRUSTED: PV2 ( a b -- b a ) ;' or 'TRUSTED: PV3 ( [ n -- n ] -- ) ;' makes MULTI-ERR-END return 1 with no diagnostic printed, yet the word is published (a later ': PV2 ( -- ) ;' dies 'duplicate definition: PV2', rc 78). Loaded alone, both declarations are admitted (rc 0). Measured on the public hb (installed 2026-10-30 20:50) and on the release stack baseline lqwprrlr alike. Cause per the pdlatch review: MULTI-REFUSED counts an unjudged scan's rejected body while DIAG-QUIET suppresses its report. Probes: /tmp/rev-fix2/p/sig2.f, sig3.f (expected output .base.out); single-load comparison /tmp/jerry-lead-tmp/pv/one2.f, one3.f. Acceptance: the count matches the diagnostics printed: a declaration the plain load admits adds 0 and is published; a refused one prints its diagnostic, adds 1 and is not published. Regression row through the real multi-error load path (test/program-diagnostics or its multi-error row). Files: src/core/checker.f (MULTI-REFUSED, DIAG-QUIET), the scan that judges TRUSTED: rows. Verify: the two probes print 0; the multi-error rows stay green. Depends: none. Ownership: checker multi-error accounting. Claim: unassigned.
