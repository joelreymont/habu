---
title: Check the test helpers that need no trust
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T13:08:03.786419+02:00"
closed-at: "2026-10-31T14:16:13.250769+02:00"
close-reason: "Landed 78505034: the 17 test helpers are : definitions. Full suite 510/510 and x86-64 cross-build rc 0 on the batch stack."
---

Problem: 17 test TRUSTED: words check as : (census tc3): catch-frame 8 THROW-*, top-row-storage TS-R1/TS-R2, engine-suite ES-CPX-BAD-RUN/BAD-EXT-RUN, source-unit-range-e2e AFTER/LOAD/CHECK-ABSENT/RUN, memory-guarded-failure FILTER-IN-PROG. Acceptance: all :, the native code of the THROW-* still pushes before throw, rows pass. Files: test/catch-frame.f, test/top-row-storage-test.f, test/engine-suite.f, test/source-unit-range-e2e.f, test/memory-guarded-failure.f. Verify: rows catch-frame, top-row-storage, engine (whitebox), source-unit-range, memory-guarded-failure. Depends: none. Ownership: those files. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tc3
