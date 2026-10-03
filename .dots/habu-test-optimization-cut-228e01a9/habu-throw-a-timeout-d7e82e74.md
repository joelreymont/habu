---
title: "Throw a timeout when a pty wait's clock ends"
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-01T11:56:01.715787+02:00\""
closed-at: "2026-10-01T17:16:44.042176+02:00"
close-reason: Fixed by 7377f5ea (review 167 ACCEPT)
---

Problem: lib/pty-harness.f WAIT-FOR/WAIT-* return false when their clock ends with the child alive (~:421-434, :453; test/proc-pty.f WAIT-PROMPT ~:110), so a slow child that is not hung fails a 20 s stream assertion and its row reads kind=exit (a failure), not TIMEOUT-UNDER-LOAD; the round-4 exit contract labels a deadline as a timeout. Evidence: review 122 $HOME/.cache/tmp/kestrel-r4-rev122/late.out/.rc (prompt at 25 s -> F1, rc 1). Acceptance: a wait whose clock ends while the child lives throws E-PROC-TIMEOUT (pool label TIMEOUT-UNDER-LOAD); a wait that ends on hang-up (READ-STEP 0 <) still returns false; WAIT-BARRIER's internal use keeps its contract; a case with a child that answers late is seen failing first; proc-pty, pty-harness-test and repl-address-cell-rollback pass. Files: lib/pty-harness.f, test/proc-pty.f, lib/pty-harness-test.f.
