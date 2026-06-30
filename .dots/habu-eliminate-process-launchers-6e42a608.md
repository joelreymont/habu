---
title: Eliminate process launchers from test suite
status: active
priority: 2
issue-type: task
created-at: "2026-06-30T01:39:47.353637+02:00"
---

Problem: native test suite still spends wall time crossing process boundaries. Shared setup builds warm/test binaries, but many tests still launch bin/hb/tool subprocesses instead of calling checked in-process test words. Root cause: CLI contract tests and tool fixtures are modeled as launcher invocations, so setup caching cannot remove spawn cost. Fix: refactor tools/tests into lib entry words plus thin CLI entry files, add named sequential/parallel test groups that call libs in-process, keep only true CLI smoke tests for argv/env/process-boundary coverage, and delete redundant process-launcher tests. Acceptance: process-launch count is measured by the test suite, ordinary semantic tests perform zero hb child launches, remaining launcher tests are explicitly named boundary tests, and public host-class timing profiles prove no regression.

2026-06-30 local checkpoint: `lint-tools` warm-runner dispatch now calls a shared in-process setup plus `test/gate-stdlib-lint-tools.f`; live lint words (`REPL-LINT`, `TRUST-LINT`, `STALE-STATUS-LINT`, `DOT-DEP-LINT`, `MAKI-DEP-LINT`) run directly and CLI wrappers stay in the standalone `test/gate-stdlib.f` path. Fixed `test/gate-stats-test.f` to save/restore `GS-PATH$` so resident tests can keep recording spans after the fixture deletes its temp root. Proof on macOS/aarch64 only: focused standalone `lint-tools` pass 14.01s wall; direct warm-runner `lint-tools` pass 6.437s internal / 13.84s cold shell wall; full hot native suite pass 42.217s internal / 44.61s wall, `inner-hb=42`, `boundary=59`, `warm-miss=0`. Remaining local work: `tail`, dictionary/checker, and semantic tool group critical paths still dominate; zed/device dots intentionally untouched.
