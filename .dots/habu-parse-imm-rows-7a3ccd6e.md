---
title: parse-imm rows outlive a rolled-back scope while symbol ids are reused
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:58:46.557301+02:00"
---

Problem: src/core/checker.f's comment on the PIMM table says Symbol-keyed and never rolled back: a retired symbol id is never reused. SYM-REWIND lowers SYM-N on a rollback and SYM-INTERN hands the ids out again, and PIMM-N is not in the rollback frame (RBF-SAVE-INTO, RBF-RESTORE-FROM), so a parse-imm row made inside a scope that rolls back stays keyed to an id another word then takes. Whether a load reaches this today is not shown; the fix of habu-the-pre-pass-3587fdd1 has the verifier add rows inside each --batch subject's scope, where it is reached. Found by that fix's worker (briefs8/reports/immfix-stop.md). Source: Joel: "i want all issues identified to be dotted and fixed". Acceptance: PIMM-N is saved and rewound with the frame as DFER-END is, the comment says what happens to a row on a rollback, and a --batch run of two subjects shows the second not reading the first's row. Files: src/core/checker.f. Verify: that case in test/diag-position-test.f or the batch test. Depends: none; lands in the commits of habu-the-pre-pass-3587fdd1. Ownership: the checker's parse-imm table. Claim: unassigned.
