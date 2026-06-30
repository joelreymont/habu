---
title: Split gate runner into importable dispatch lib
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-30T21:23:24.838264+02:00\""
closed-at: "2026-06-30T21:40:53.750164+02:00"
close-reason: "done: test/gate-runner-lib.f loads without dispatch, test/gate-runner-entry.f tail-runner path passes, filemap-lint and typed-local-diff-lint pass"
---

Problem: test/gate-runner-entry.f is side-effect-only and runs GR-MAIN at load time, so test/run.f cannot call runner phases from the resident suite process. Fix: move constants/token parsing/GR-DISPATCH/GR-STATS/GR-PASS/GR-MAIN into new test/gate-runner-lib.f with a public callable dispatch word such as GR-RUN-ID/GR-RUN-TOKEN; keep test/gate-runner-entry.f as a thin CLI entry that requires the lib and calls GR-MAIN. Update FILEMAP/filemap-lint/content keys. Acceptance: existing warm-runner CLI behavior unchanged; direct load of the lib does not execute a phase; focused runner entry invocation still passes for a cheap token; this enables resident dispatch in test/run.f without loading side-effect CLI entry.
