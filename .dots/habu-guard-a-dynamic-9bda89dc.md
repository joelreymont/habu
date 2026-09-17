---
title: "Guard a dynamic buffer's control head from user stores"
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T19:03:54.540453+03:00"
---

Problem: habu-fix-the-definers-cc06e107 (2026-09-17) makes DYNAMIC-BUFFER's generated NAME#base a PTR-VARIABLE, so the base word publishes ( -- ptr ptr a ) instead of a raw ( -- ptr a ); it is bare-executable on a sealed engine already (IMK-WALK exempts DKIND:ADDR records), so a program naming NAME#base can now store or fetch a pointer in the control head that DYNAMIC-STORAGE:RESERVE/RELEASE own, where before it could only hold a scalar. Acceptance: the generated control head is not writable from user code (REG-PROTECT on the generated base, or the base word made private to the generated accessors), measured by a rejected program that stores through NAME#base, with the definer suites, the capture suites and the byte fixpoint green; the cost at user declaration time measured. Files: src/core/layout-buffer.f (DBUF-SOURCE), src/core/dynamic-storage.f, test/. Verify: the rejected fixture; certify-dynamic-buffer.f and the dynamic-buffer suites; fixpoint; test/run.f. Depends: habu-fix-the-definers-cc06e107. Ownership: definers. Claim: unassigned.
