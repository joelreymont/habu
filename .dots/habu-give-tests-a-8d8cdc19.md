---
title: "Retire the TRUSTED: evaluate wrappers left in tests"
status: open
priority: 2
issue-type: task
blocks:
  - habu-add-the-evaluate-12ffa000
created-at: "2026-09-16T16:57:00.779496+03:00"
---

Problem: four test words still wrap `evaluate` in a TRUSTED: body, the unchecked definer that habu-delete-the-trusted-42b30edd removes. test/top-row-storage-test.f:31 TS-EVD and :34 TS-Z evaluate on the pool stacks evaluate-closed hands out (TS-R1, TS-R2); their case checks that a finished evaluation's segment does not displace a live one. test/engine-suite.f:1515 ES-CPX-BAD-EVAL and :1516 ES-CPX-BAD-EXT-EVAL check that a definition refused inside evaluate throws 70 and rolls code space and the dictionary back (EM-EVAL-THROW-RECOVER). Every other evaluate wrapper in the tests already runs through the evaluate-closed primitive or the checked words of src/lib/test/eval.f (package TEST-EVAL: N, FLAG, RC). Texts that embed a TRUSTED: evaluate word inside a child program or an evaluated string (test/top-row-storage-test.f:362 and :441, test/diag-position-test.f:1731, test/underdepth-gate.f:295, test/load-reject-diag-test.f:789, test/compiler/native-lexical-recovery.f:45) exercise TRUSTED: itself and go with 42b30edd.

Acceptance: the four words are checked code (evaluate-closed, TEST-EVAL, or a top-level evaluate), and each case still checks what it checks now: the order of pool-stack segments, and the rollback of the plain evaluate frame after a refused definition. A case that cannot keep its meaning without TRUSTED: is reported with the reason and added to 42b30edd as an acceptance case. Both files' case counts are unchanged.

Files: test/top-row-storage-test.f, test/engine-suite.f; src/lib/test/eval.f only if a needed checked word is missing.

Verify: `rg -nU 'TRUSTED: [^;]*\bevaluate\b' test` lists only the embedded texts named above; the two suites pass with their case counts unchanged.

Depends: none. Part of habu-trusted-dies-prim-4fd12d60.

Ownership: unassigned. Claim: unassigned.
