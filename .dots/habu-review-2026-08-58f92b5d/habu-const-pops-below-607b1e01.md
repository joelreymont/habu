---
title: constant pops below the interpret stack base before any guard
status: closed
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.218842+02:00"
closed-at: "2026-09-16T14:34:48.734363+03:00"
close-reason: "superseded by habu-campaign-c4-diagnostics-3b6de147: constant still pops a value before any depth guard, so an empty stack underflows past the interpret base and leaves checker rows behind"
---

Problem: habu2.f:3184 '15 G-POP' in C-CONSTANT; keyword rows bypass EM-INTERPRET-FIND's LARITY/LMININ gates (6756-6761) and the depth-floor guard at EM-COMMENT 6081 fires only on the next token - after NDICT is bumped, LHIDXADD (3192) and the checker publish (3194-3195); at top level the process exits 70 naming the constant, inside evaluate the rollback undoes NDICT but not the checker rows PUBLISH-A/C-DEFHOOK registered. Acceptance: XDS compared with S0-CELL before the pop, taking the LUNDERFLOW leg; a test 'constant X' on an empty stack inside evaluate shows no row registered; mirror follows. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, test/. Verify: the test. Depends: none. Ownership: reader. Claim: unassigned.
