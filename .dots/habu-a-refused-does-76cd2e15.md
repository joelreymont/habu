---
title: A refused does> definer in a multi-error load stops the load in the native compiler
status: open
priority: 1
issue-type: task
created-at: "2026-11-10T20:13:39.391199+02:00"
---

Problem: inside MULTI-ERR-BEGIN .. MULTI-ERR-END a definer the checker refuses ends the whole load instead of being counted and skipped. Two observations, cause not hunted. (1) At tier 1 ('1 set-tier', as test/native-multi-error-recovery.f runs) the definer of dot habu-count-only-refusals-e8b5dd42's reproducer 2, whose head is refused, dies 'ncomp: cannot compile DEFR' with an uncaught -8575 (E-COMP-BINDING). (2) On master, a definer whose does> clause is refused, ': DQ4 ( -- ) create 0 , does> ( -- n ) drop ;' and its TRUSTED: form, ends the load rc 70 with 'ncomp: cannot compile DQ4 at drop'. test/native-multi-error-recovery.f CHECK-CLAUSE-DEFINER asserts a rolled-back definer for a refused clause, so one of the two paths differs from what that test covers. Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: in a multi-error load each of the two programs is counted once, rendered once, rolled back, and the load goes on to its next definition, at both tiers. Depends: none; touches the same words as cause B of habu-count-only-refusals-e8b5dd42. Claim: unassigned.
