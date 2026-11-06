---
title: Report a failed fixture build in file-objects
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:17:09.336323+02:00"
---

Problem: test/file-objects.f CHECKS runs A$ RUN-PROG right after build A; when build A fails it wrote no program, so RUN-PROG's spawn throws E-PROC-SPAWN (-2500) uncaught and the harness exits before T-REPORT, hiding which labels failed (seen by review a6f0d228 with linker.f's rule 5 removed: rc 67, no report). Acceptance: when build A's rc is nonzero the harness skips RUN-PROG and every check that reads A's objects, records the build's failure as TFAIL labels and reaches T-REPORT; a control (a fixture made to fail) shows the report. Files: test/file-objects.f (CHECKS, RUN-PROG). Verify: HABU_UNDER_TEST=$PWD/bin/hb bin/hb --load test/file-objects.f, passing and with a failing control. Depends: none (merge with the stop lane and L2d, which edit the same file). Ownership: test/file-objects.f. Claim: unassigned.
