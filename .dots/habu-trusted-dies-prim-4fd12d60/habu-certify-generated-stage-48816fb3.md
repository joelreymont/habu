---
title: Certify generated stage source again
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T19:32:23.873474+03:00"
---

Problem: tools/build-fixpoint-test.f is red on the root (fc23ff28 and later, root-built engine, untouched tree): E-BUILD-CERTIFY 'generated stage source rejected' (throw -2805) at asserts 12-16, 19, 21; it fails identically with the tier line in the prelude, with the old 1 set-tier line, and with no tier line at all, so VERIFY:SOURCE-BUF rejects the generated stage source for a reason that predates the tier stack (the tier lane could therefore not prove the prelude guard through certification; its evidence is the cold checkpoint). Related: the build-fixpoint refresh dies at BF-CERTIFY-PREFIX and the prefix certify rejected 'ptr a n' earlier today (cedar's b0b90daa repaired part of it). Acceptance: the exact rejected token and rule named; the generated source certifies again (rc 0) with no rule weakened; tools/build-fixpoint-test.f green; tools/build-fixpoint-refresh.f -- install runs to completion from the root. Files: tools/build-fixpoint.f, tools/build-fixpoint-test.f, src/core (VERIFY:SOURCE-BUF, CHECKER-CALLS). Verify: the test and the refresh. Depends: none. Ownership: rowan (build tools) or a hazel handoff. Claim: unassigned.
