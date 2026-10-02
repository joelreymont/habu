---
title: Make the test capture reentrant and cloexec
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T11:02:08.052179+02:00"
---

Problem (r4-gecapture, 8f78f78e fold 8b8b7adf): lib/test/runner.f GT-CAPTURE-ACTION keeps its capture paths and saved descriptors in process-wide cells (GT-CAP-OUT-SAVE and neighbours), so a capture nested in another capture's action truncates the outer one's files (documented, not guarded); the saved stdout/stderr are dup'd with F_DUPFD, not close-on-exec, so a child the action spawns inherits the real streams at fd >= 10; a child the action leaves running can write after the size check, and READ-ALL then throws E-FS-CAPACITY instead of E-PROC-TRUNCATED. Acceptance: the capture's state is per call (locals or a frame), so a nested capture works and both captures read their own bytes; the saved descriptors are close-on-exec (per-target F_DUPFD_CLOEXEC or equivalent) and a spawned child sees neither; output growing after the check is reported as E-PROC-TRUNCATED; cases in test/gate-common-test.f seen to fail first; IN-PROC and GE-CAPTURE-ACTION rows rc 0. Files: lib/test/runner.f, test/gate-common-test.f, docs/stdlib.md. Base: after 8f78f78e (8b8b7adf) lands.
