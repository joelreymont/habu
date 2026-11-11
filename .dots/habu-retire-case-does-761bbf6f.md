---
title: RETIRE-CASE does not assert that build three links
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:30:41.275786+02:00"
---

Problem: test/file-objects.f RETIRE-CASE (Z, Z2, Z3; added by 'Give a changer no stores time to keep') asserts only what the built program prints, so Z3 passes whether build three links r.f or recompiles every file; nothing checks that the stores-time fix leaves an unchanged third build warm. The case runs three fresh-process hb builds (the form NEST-CASE and SAME-CASE use); its wall and CPU cost was not measured. Found by the storer-f1 re-review, finding 5. Consumers: none meets it. Acceptance: Z3 fails when r.f is compiled on build three (a compile count from the build, or the in-process form in test/fixtures/file-objects/build.f beside BY-GAIN if it can show the printed K), and the case's wall time is recorded before and after. Files: test/file-objects.f, test/fixtures/file-objects/build.f. Verify: bin/hb --load test/file-objects.f rc 0; with the fix reverted in a scratch tree Z2 fails. Depends: the storer-time stack landing (dot 48b4760e). Ownership: test/file-objects.f RETIRE-CASE. Claim: unassigned.
