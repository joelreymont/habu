---
title: Make the checker-scan differential one pass
status: open
priority: 1
issue-type: task
created-at: "2026-09-29T18:03:11.941658+02:00"
---

Problem: test/checker-scan-index-suite.f and checker-scan-index-rollback-suite.f run SCX-DIFF-ALL 4x per row; each compares every one of 17,122 symbols against USIG-NEWEST-LINEAR, a full walk of 20,823 records (11 s per pass solo; section 5 grows the table to 32,769). Evidence and design: ~/.cache/tmp/kestrel-gate/test-review/L4-checker.md (checker-scan finding): one pass builds a newest-per-symbol map bounded by UEND and the comparison stays independent of the index under test. Also delete the rollback row's SCX-SEALED case (package privacy, not the index). Acceptance: the differential still fails when an index entry is wrong (mutation shown on each index kind); both rows drop to fixture cost. Files/Ownership: test/checker-scan-index*.f, their registry rows. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. These are WHITEBOX rows: run them through a mini registry as in ~/.cache/tmp/rs-cs-split-registry.f. Depends: none. Claim: unassigned.
