---
title: Agree on the code for an uncompilable token after exit
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:39:18.075991+02:00"
---

Problem: ': RWZ1 ( -- ) exit immediate ;' and ': RWZ2 ( -- ) exit rwx-nowhere ;' are E-DEAD-CODE under tools/check.f but E-UNDEFINED in the load (probes /tmp/tim-dots/f14.f, f14b.f; both rc 70). The checker latches DEADERR at the first non-closing token after exit (src/core/checker.f:23171) and REFUSAL-OF ranks RF-DEAD first (:5442), while the load's compiler still compiles the dead token and fails on it. Acceptance: one code for the same source on both paths (the token the load cannot compile is named the way the load names it, or the load reports dead code first), with a row for each probe. Files: src/core/checker.f, src/core/render.f, test/load-reject-diag-test.f. Verify: the probes. Depends: none. Ownership: checker diagnostics. Claim: unassigned.
