---
title: five assertion vocabularies and duplicated gate bookkeeping
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.056996+02:00"
---

Problem: lib/test/assert.f (T=), lib/test/runner.f:108-127 (GT-CHECK-N/GT-CHECK$), test/gate-common-lib.f (GE-FAIL/GE-EXPECT-*), test/gate-build-common.f (N=) plus per-file clones: 31 test files define their own T-FAIL, 30 their own T=, 6 their own T$=, 33 private #FAIL/#CASE counters, two private CONTAINS? (engine-suite.f:575-587, type-decl-suite.f:40-53); 781 TRUSTED:/0 set-check sites in 81 test files; 56 test files open no package (docs/forth.md Testing requires one); 242 SUITE registrations in test/gate-stdlib-cases.f versus 332 GSI-scheduled paths with 11 'listed here as well' comments - two sources of truth whose drift is the schedule-lint finding; runner dead weight: TR-GROUP-MODE always PAR (test/run-lib.f:1309-1314) so GROUP-SEQ? and test/run-resident.f:61-66 never run, TR-PHASE-TOOLS-ENV is drop (:1152), TR-PRE-*-START empty (:1479-1483), five case tables over a 41-id phase space edited in lockstep. Acceptance: one assertion vocabulary, clones deleted; one registration table that the runner consumes; dead runner arms removed; the 56 files packaged; the TRUSTED census in tests printed and each site owned. Files: lib/test/, test/run-lib.f, test/gate-stdlib-cases.f, test/gate-stdlib-inline-lib.f. Verify: full gate; schedule-lint. Depends: the schedule-lint dot. Ownership: test harness. Claim: unassigned.
