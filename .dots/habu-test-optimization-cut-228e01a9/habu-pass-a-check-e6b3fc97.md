---
title: "Pass a check run's standard output through uncapped"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T13:49:03.883215+02:00"
---

Problem (lane 319 r4-chkout, 9ef2f891): check.f captures the run's stdout into CHK-OUT-CAP (32 KiB) only to replay it, so a checked program that prints more is refused (rc 70) though it is valid; stderr must stay captured (check.f rewrites the subject's name in it and filters it for JSON). Acceptance: lib/process.f gains a capture that captures stderr only and lets stdout through to check.f's own stdout in order; check.f uses it; a fixture printing 1 MB checks rc 0 with all its output; the stderr cap refusal stays. Files: lib/process.f, tools/check-core.f, tools/check-test-lib.f, docs/stdlib.md.

Review 331 (of 9ef2f891): keep CHK-RUN-CAPPED; CHK-RUN-TOO-BIG drops its stdout clause ("the run wrote past its capture of 131072 bytes of standard error"); OUT-OVER$ in check/run-output-cap becomes a pass case asserting rc 0 and the full output; EXPECT-RUN-OVER keeps the stderr case. check.f already replays stdout before stderr (CHK-HANDLE-HB), so no reorder; a stderr-cap or deadline refusal would then follow partial live stdout instead of empty stdout: own that with a test. Shared library change: the full native suite applies.
