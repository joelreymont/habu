---
title: "INCIDENT: master red - prop-test rejects schema-n@ candidate"
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T21:38:30.589665+02:00"
---

STOP-EVERYTHING (2026-08-20): test/prop-test.f fails on master b5e5cdb3 - 'primitive candidate rejected: : PROP-PC ( -- n ) schema-n@ ;' + 'primitive semantic case failed', checker complaint inside the generated word (at dup expected: i64 actual: i64 i64). Bisected by ir-1: passed at d98d1d7a (the schema seal), broken by 9f598292 (the NIMM deletion) - which touches no schema file, so the mechanism is candidate-pool shift: deleting NIMM's words moved prop-test's selection onto SCHEMA-N@, whose post-seal state (axiom relocated to PPRIM: SCHEMA-REG at d98d1d7a) fails the semantic leg on first contact. The seal may have left the bare spelling enumerable with a stale/mismatched effect row, exposed only when selection landed on it. SECOND DEFECT in the same incident: my merge gate ran test/run.f green on the exact red tree - prop-test is in test/run-files.f:97 but evidently not in the resident execution path; the wired-is-not-runs drift is now incident-grade. FIX BOTH: the schema-n@ effect mismatch at its root (why didn't the relocation carry the effect - or why is the bare spelling still enumerable), AND register prop-test in the path the gate actually executes. No merges until master green.
