---
title: "Find why crew-test's waits expire under load"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T19:14:37.368602+02:00"
---

Problem: src/lib/crew-test.f WAIT (:46-47) polls for 10 s (WAIT-MS :26) and the test ends 'crew-test: a deadline expired' / 'hb: uncaught throw code -2502', exit 67. Observed: full suite at rvtyrpsk fff7fcb1 (2026-11-06, carl-land/.out/suite2-run1.log :455-461, WAIT at 10033 ms with cpu 787 ms, so the test sat blocked, not computing); and with four crew-test processes run at once during a full suite, one of four failed this way on the installed hb (gen3 from db4aff9b, so not the OPTION:SOME change) and a second build failed the RECORD-CASE way that dot habu-make-crew-test-11bf07b0 covers. Alone it passes in 0.8 s (3 of 3). Unknown: which wait expires (the label is not printed) and whether a task is starved or a wake is lost. Hypotheses to test: a lost TASK:WAIT wake in src/lib/crew.f or src/lib/task.f (predicts the expiry still happens with WAIT-MS at 60 s), or late thread start under load (predicts it passes at 60 s). Reproducer: cd a workspace with bin/hb; for i in 1 2 3 4 5 6 7 8; do (TMPDIR=$(mktemp -d) bin/hb --load src/lib/crew-test.f </dev/null >/tmp/crew-$i.out 2>&1; echo $i rc=$?) & done; wait - under a running full suite. Acceptance: the cause named with the evidence; the fix as a second brief. Files: src/lib/crew-test.f, src/lib/crew.f, src/lib/task.f. Verify: bin/hb --load test/run.f -- crew. Depends: none. Claim: unassigned.
