---
title: Cut rerun rows and repeated windows in checker tests
status: closed
priority: 1
issue-type: task
created-at: "2026-09-29T18:03:11.949235+02:00"
closed-at: "2026-09-29T23:07:09.044049+02:00"
close-reason: "Landed 46562b1e (prop sweep 2 shards x 2000; independent review PASS). Item 1 was already done by 5c0d15fc. Item 2 declined with evidence: field-proj-boundary cases 1-2 catch failures case 3 misses (TRUST-DECL mutations) and the row's cost is case 3's tier-1 compile, which is its subject. Gate 503/503."
---

Problem: test/program-diagnostics-test.f DIAGNOSTICS re-runs whole registered rows for one stderr needle (engine-suite.f alone 18.2 of 22.6 s); test/field-proj-boundary.f recompiles the window three times with identical assertions (cases 1-2 subsumed by 3); test/prop-test.f forks 8 shards inside the 8-slot pool (47 s CPU at 708%). Evidence: ~/.cache/tmp/kestrel-gate/test-review/L4-checker.md. Acceptance: 1-3 line fixtures carry each diagnostic needle; one window compile serves field-proj; prop-test keeps 4,000 programs in 2 shards or sequential. Files/Ownership: those three files and their registry rows. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: unassigned.
