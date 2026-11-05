---
title: "Delete the checker's test-only ASIG words"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T00:11:28.370384+02:00"
---

Problem: src/core/checker.f CHECKER-ASIG-KNOWN? (:13971), CHECKER-ASIG-MISSING? (:13978) and CHECKER-ASIG-ROW-FOR (:13986), with their PRIM rows (:11504-11506), are reached only by test/name-length-test.f:112-115 (ASIG-NAME, ASIG-PKG, ASIG-MISSING, ASIG-ROW) and test/checker-surface.f:39; the audit asks AOT-ARM:PAYLOAD-LOOKUP (src/habu/aot-capture.f ACAP-?SIG). The PRIM rows of CHECKER-ASIG-ARM, -DISARM and -ARMED? (:11494-11496) have no checked caller outside checker.f (src/habu/aot-arm.f arms through owner-ABI offsets), and the axiom comment above the KNOWN?/MISSING? rows ('the audit's two questions') is stale. Lines as at the pruning stack's tip (zpmzulux). Acceptance: the three words, their PRIM rows and the test cases that only probe them are deleted; the ARM/DISARM/ARMED? PRIM rows go when hb builds and the checker tests pass without them, else the lane names the refusal; no comment names a deleted word. Files: src/core/checker.f, test/name-length-test.f, test/checker-surface.f. Verify: a native build of hb from the tree, then bin/hb --load test/run.f -- name-length checker-surface engine type-decl. Depends: the pruning stack's landing (habu-keep-only-the-4b091d0e). Ownership: the checker's AOT signature pool. Claim: unassigned.

Also stale, same lane: the registry-verbs axiom block (src/core/checker.f:11499-11502 at the stack tip) names src/habu/aot-arm.f and src/habu/aot-capture.f as callers of the CHECKER-REG-AOT-* words, which neither file names (they are reached through CHECKER-PAYLOAD-ARM/-FREEZE and the owner-ABI xt stores, checker.f:31341-31346), and says "two capture-side verbs" above three PRIM rows. The lane deletes the rows nothing checked calls and corrects the comment.
