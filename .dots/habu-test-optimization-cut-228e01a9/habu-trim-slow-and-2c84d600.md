---
title: Trim slow and trivial library tests
status: open
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:21.067046+02:00"
---

Problem: object-source-resolver sizes one 528 KiB payload to a codec ceiling that no longer exists (15.4 s; $10001 passes in 1.5 s); curl-http spends 5.3 of 5.7 s in deliberate stalls; process-tasks spawns 240 children for three regressions; process-cwd EARLY-EOF duplicates process-test.f:383-392; pty-harness runs two REPL children where one serves; the pg row asserts nothing without HABU_PG_CONNINFO; float-test mirrors private helpers (60-124); unicode dataset pins; property-test cases for unused words; obvious fork-contract cases. Evidence: ~/.cache/tmp/kestrel-gate/test-review/L7-lib.md. Acceptance: same failure detection with the listed cuts; relocation asserted explicitly in object-source-resolver; pg row removed from the registry; library deletion is NOT in scope (19 libraries without consumers need the owner's decision). Files/Ownership: lib/**/*-test.f named in the report, tools/image-bytes-test.f excluded, registry rows. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: unassigned.
