---
title: Eliminate process launchers from test suite
status: active
priority: 2
issue-type: task
created-at: "2026-06-30T01:39:47.353637+02:00"
---

Problem: native test suite still spends wall time crossing process boundaries. Shared setup builds warm/test binaries, but many tests still launch bin/hb/tool subprocesses instead of calling checked in-process test words. Root cause: CLI contract tests and tool fixtures are modeled as launcher invocations, so setup caching cannot remove spawn cost. Fix: refactor tools/tests into lib entry words plus thin CLI entry files, add named sequential/parallel test groups that call libs in-process, keep only true CLI smoke tests for argv/env/process-boundary coverage, and delete redundant process-launcher tests. Acceptance: process-launch count is measured by the test suite, ordinary semantic tests perform zero hb child launches, remaining launcher tests are explicitly named boundary tests, and public host-class timing profiles prove no regression.
