---
title: "Run the C2 record cases in the suite's process"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T20:40:26.504837+02:00"
---

Problem: test/c2-records-e2e.f, c2-field-loan-e2e.f, c2-memory-e2e.f and c2-view-record-e2e.f run case files as children of bundle variants (hb-root, hb-ordinary) copied by test/c2-image-pair.f; hb-root is written by the same writer as bin/hb (NATIVE-EMIT:WRITE-C2), so the children prove nothing bin/hb in-process cannot, except the saved-image cases (APP-IMAGE:SAVE then running the saved image), whose subject is the save boundary. Acceptance: lead design after the census: source and tier-1 cases run in the suite's process on bin/hb; saved-image cases keep one child with the boundary stated beside it; hb-ordinary cases either prove a shipped behavior in-process or go; C2-IMAGE-PAIR and the bundle go when no consumer remains. Files: the four e2e files, their case files, test/c2-image-pair.f, test/compiler-images*.f, test/run.f bundle rows. Verify: the four rows green on bin/hb; rg C2-IMAGE-PAIR returns nothing. Depends: census. Ownership: those files. Claim: unassigned.

2026-10-30 21:00, census A: c2-records-e2e starts 8 children; its five source cases (:30-53) have no boundary and go into the row as test/c2-read-effects.f and the other C2 rows already are (test/gate-stdlib-cases.f:1509-1536); the save (:56-62) and the two saved-image runs (:67-80) keep their children. The other C2 e2e rows have the same shape (about 25 engine starts). hb-root proves nothing bin/hb does not. hb-ordinary (NATIVE-EMIT:WRITE, which grants no C2 authority) is a negative control for a writer nothing shipped uses (callers: the bundle :1386 and three test fixture writers); the stated reason for the pair is that the C2 grant comes from the writer with the capture fixed (gate-stdlib-cases.f:124-125, c2-field-loan-ordinary.f:1). Lead ruling: hb-ordinary and its cases go with hb-root; the bundle keeps only hb-whitebox-source, which the whitebox rows need because the product strips checker internals.
